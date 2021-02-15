[<AutoOpen>]
module Chipper.Web.Util

open System.Threading.Tasks

let await (task : ValueTask<'a>) = task.AsTask() |> Async.AwaitTask
let await' (task : ValueTask) = task.AsTask() |> Async.AwaitTask
