module MongoDB.FSharp

open MongoDB.Bson.Serialization.Serializers
open MongoDB.Bson.Serialization.Conventions
open Microsoft.FSharp.Reflection
open System.Reflection
open System.Linq.Expressions
open MongoDB.Bson.Serialization
open System

let private isOption typ = FSharpType.IsUnion typ && typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<_ option>

module Serializers = 
    
    type OptionTypeSerializer<'T>() =
        inherit SerializerBase<'T option>()
    
        override __.Serialize(context, args, value) =
            match value with
            | Some x -> BsonSerializer.Serialize<'T>(context.Writer, x)
            | None -> BsonSerializer.Serialize<obj>(context.Writer, null)

        override __.Deserialize(context, args) =
            let value = BsonSerializer.Deserialize<'T> context.Reader
            if box value |> isNull then None else Some value

    let register () =
        let serializationProvider = 
            { new IBsonSerializationProvider with
                    member __.GetSerializer typ = 
                        match typ with
                        | _ when isOption typ ->
                            typedefof<OptionTypeSerializer<_>>.MakeGenericType (typ.GetGenericArguments())
                            |> System.Activator.CreateInstance
                            :?> IBsonSerializer
                        | _ ->
                            null }
        BsonSerializer.RegisterSerializationProvider serializationProvider


module Conventions = 

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

                if isOption typ then
                    memberMap.SetDefaultValue None |> ignore
                    memberMap.SetIgnoreIfNull true |> ignore


    type UnionTypeConvention() =
        inherit ConventionBase()

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
                        FSharpType.GetUnionCases typ |> Seq.item 0 |> mapCase classMap
        
    let register () =
        let pack = ConventionPack()
        pack.Add(RecordTypeConvention())
        pack.Add(UnionTypeConvention())
        pack.Add(OptionTypeConvention())
        ConventionRegistry.Register("F# Type Conventions", pack, fun _ -> true)