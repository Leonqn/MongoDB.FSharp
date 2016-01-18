module public MongoDB.FSharp.Serializers

open MongoDB.Bson.Serialization.Serializers
open MongoDB.Bson.Serialization

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
                    | _ when FSharpType.IsOption typ ->
                        typedefof<OptionTypeSerializer<_>>.MakeGenericType (typ.GetGenericArguments())
                        |> System.Activator.CreateInstance
                        :?> IBsonSerializer
                    | _ ->
                        null }
    BsonSerializer.RegisterSerializationProvider serializationProvider