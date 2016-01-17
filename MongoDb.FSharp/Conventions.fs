namespace Mongodb.FSharp

open MongoDB.Bson.Serialization.Conventions
open Microsoft.FSharp.Reflection
open System.Reflection
open System.Linq.Expressions
open MongoDB.Bson.Serialization
open System

type RecordTypeConvention() =
    inherit ConventionBase()

    interface IClassMapConvention with
        member __.Apply classMap =
            let typ = classMap.ClassType

            if FSharpType.IsRecord typ then
                let fields = FSharpType.GetRecordFields typ
                let names = fields |> Array.map (fun x -> x.Name)
                let types = fields |> Array.map (fun x -> x.PropertyType)

                let ctor = typ.GetConstructor types
                classMap.MapConstructor(ctor, names) |> ignore

                fields |> Array.iter (fun x -> classMap.MapMember(x) |> ignore)

type OptionTypeConvention() =
    inherit ConventionBase()

    interface IMemberMapConvention with
        member __.Apply memberMap =
            let typ = memberMap.MemberType

            if FSharpType.IsOption typ then
                memberMap.SetDefaultValue None |> ignore
                memberMap.SetIgnoreIfNull true |> ignore

type UnionTypeConvention() =
    inherit ConventionBase()

    let get index array = Array.get array index

    let isUnion typ = FSharpType.IsUnion typ

    let makeDelegate (meth : MethodInfo) =
        let types = meth.GetParameters() |> Array.map (fun x -> x.ParameterType)
        Expression.GetDelegateType(Array.append types [| meth.ReturnType |])

    let mapCase (classMap : BsonClassMap) (case : UnionCaseInfo) =
        let fields = case.GetFields()
        let names = fields |> Array.map (fun x -> x.Name)

        classMap.SetDiscriminatorIsRequired true
        classMap.SetDiscriminator case.Name

        let ctor = FSharpValue.PreComputeUnionConstructorInfo(case)
        let del = Delegate.CreateDelegate(makeDelegate ctor, ctor)

        classMap.MapCreator(del, names) |> ignore

        fields |> Array.iter (fun x -> classMap.MapMember(x) |> ignore)

    interface IClassMapConvention with
        member __.Apply classMap =
            let typ = classMap.ClassType

            if typ.DeclaringType <> null && isUnion typ.DeclaringType then
                FSharpType.GetUnionCases typ
                |> Array.find (fun x -> x.Name = typ.Name)
                |> mapCase classMap

            elif isUnion typ && not typ.IsAbstract then
                let nested = typ.GetNestedTypes() |> Array.filter isUnion
                let props = typ.GetProperties() |> Array.filter (fun x -> isUnion x.PropertyType)

                if nested.Length = 0 && props.Length = 0 then
                    FSharpType.GetUnionCases typ |> get 0 |> mapCase classMap