[<AutoOpen>]
module Chipper.Util

open System
open System.Threading.Tasks

let await (task : ValueTask<'a>) = task.AsTask() |> Async.AwaitTask
let await' (task : ValueTask) = task.AsTask() |> Async.AwaitTask

let (|Guid|_|) (id : string) =
    match Guid.TryParse(id) with
    | true, result -> Some result
    | _ -> None
