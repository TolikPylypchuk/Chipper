[<AutoOpen>]
module Chipper.Web.AppRouter

open System
open Bolero

let (|Guid|_|) (id : string) =
    match Guid.TryParse(id) with
    | true, result -> Some result
    | _ -> None

let router = {
    getEndPoint = fun model -> model.Page
    setRoute = fun path ->
        match path.Split('/', StringSplitOptions.RemoveEmptyEntries) with
        | [||] -> Some HomePage
        | [| "start" |] -> Some StartPage
        | [| "join"; Guid id |] -> Some <| JoinPage id
        | [| "configure" |] -> Some ConfigureSessionPage
        | [| "not-implemented" |] -> Some NotImplementedPage
        | _ -> None
        |> Option.map SetPage
    getRoute = function
        | HomePage -> "/"
        | StartPage -> "/start"
        | JoinPage id -> sprintf "/join/%O" id
        | ConfigureSessionPage -> "/configure"
        | NotImplementedPage -> "/not-implemented"
}
