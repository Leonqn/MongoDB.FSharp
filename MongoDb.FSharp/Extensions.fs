module internal MongoDB.FSharp.Extensions

open MongoDB.Driver

type IFindFluent<'TDoc, 'TProj> with

    member x.ToFSharpListAsync() = 
        async { 
            let! cursor = x.ToCursorAsync() |> Async.AwaitTask
            let rec toList list = 
                async { 
                    let! moved = cursor.MoveNextAsync() |> Async.AwaitTask
                    if (not moved) then return list
                    else return! toList (List.append list <| Seq.toList cursor.Current)
                }
            return! toList []
        }