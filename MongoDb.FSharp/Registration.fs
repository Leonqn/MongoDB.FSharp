namespace Mongodb.FSharp

module Registration =
    open MongoDB.Bson.Serialization.Conventions
    open MongoDB.Bson.Serialization

    let registerConventions () =
        let pack = ConventionPack()
        pack.Add(RecordTypeConvention())
        pack.Add(UnionTypeConvention())
        pack.Add(OptionTypeConvention())
        pack.Add(IgnoreExtraElementsConvention true)
        ConventionRegistry.Register("F# Type Conventions", pack, fun _ -> true)
    
    let registerSerializers () =
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
