[<AutoOpen>]
module Chipper.Web.Util

open Microsoft.JSInterop

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
