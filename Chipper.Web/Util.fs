[<AutoOpen>]
module Chipper.Web.Util

open System.Threading.Tasks
open Microsoft.JSInterop

let await (task : ValueTask<'a>) = task.AsTask() |> Async.AwaitTask
let await' (task : ValueTask) = task.AsTask() |> Async.AwaitTask


let writeTextToClipboard (text : string) (js : IJSRuntime) =
    js.InvokeVoidAsync("navigator.clipboard.writeText", text).AsTask()

[<RequireQualifiedAccess>]
module attr =

    open Bolero.Html
    
    let bs a value = ("data-bs-" + a) => value
    let checked' = attr.``checked``
    let class' = attr.``class``
    let for' = attr.``for``
    let role value = "role" => value
    let type' = attr.``type``
