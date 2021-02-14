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
        | [| "start"; Guid id |] -> Some <| StartPage id
        | [| "join"; Guid id |] -> Some <| JoinPage id
        | [| "configure"; Guid id |] -> Some <| JoinPage id
        | [| "not-implemented" |] -> Some <| NotImplementedPage
        | _ -> None
        |> Option.map SetPage
    getRoute = function
        | HomePage -> "/"
        | StartPage id -> sprintf "/start/%O" id
        | JoinPage id -> sprintf "/join/%O" id
        | ConfigureSessionPage id -> sprintf "/configure/%O" id
        | NotImplementedPage -> "/not-implemented"
}
