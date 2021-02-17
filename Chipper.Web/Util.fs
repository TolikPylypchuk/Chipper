[<AutoOpen>]
module Chipper.Web.Util

open System.Threading.Tasks
open Microsoft.JSInterop

let await (task : ValueTask<'a>) = task.AsTask() |> Async.AwaitTask
let await' (task : ValueTask) = task.AsTask() |> Async.AwaitTask


let writeTextToClipboard (text : string) (js : IJSRuntime) =
    js.InvokeVoidAsync("navigator.clipboard.writeText", text).AsTask()
