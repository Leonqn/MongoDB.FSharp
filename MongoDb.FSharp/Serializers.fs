namespace Mongodb.FSharp

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
        