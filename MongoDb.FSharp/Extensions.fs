module internal MongoDB.FSharp.Extensions

open MongoDB.Driver

type IFindFluent<'TDoc, 'TProj> with

    member x.ToFSharpListAsync() = 
        async { 
            let! cSharpList = x.ToListAsync() |> Async.AwaitTask
            return Seq.toList cSharpList
        }