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
        | [| "configure" |] -> Some ConfigurePage
        | [| "join"; Guid id |] -> Some <| JoinPage id
        | [| "play" |] -> Some PlayPage
        | [| "not-implemented" |] -> Some NotImplementedPage
        | _ -> None
        |> Option.map SetPage
    getRoute = function
        | HomePage -> "/"
        | StartPage -> "/start"
        | JoinPage id -> sprintf "/join/%O" id
        | ConfigurePage -> "/configure"
        | PlayPage -> "/play"
        | NotImplementedPage -> "/not-implemented"
}
