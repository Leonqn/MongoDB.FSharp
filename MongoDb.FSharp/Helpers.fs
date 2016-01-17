namespace Mongodb.FSharp

module FSharpType = 
    open Microsoft.FSharp.Reflection

    let IsOption typ = FSharpType.IsUnion typ && typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<_ option>
