[<AutoOpen>]
module Chipper.Web.AppRouter

open System
open Bolero

open Chipper.Core.Domain

let (|Guid|_|) (id : string) =
    match Guid.TryParse(id) with
    | true, result -> Some result
    | _ -> None

type CreateJoinUrl = GameSessionId -> string

let router = {
    getEndPoint = fun model -> model.Page
    setRoute = fun path ->
        match path.Split('/', StringSplitOptions.RemoveEmptyEntries) with
        | [||] -> Some HomePage
        | [| "start" |] -> Some StartPage
        | [| "invite" |] -> Some InvitePage
        | [| "join"; Guid id |] -> Some <| JoinPage id
        | [| "configure" |] -> Some ConfigurePage
        | [| "play" |] -> Some PlayPage
        | [| "not-implemented" |] -> Some NotImplementedPage
        | _ -> None
        |> Option.map SetPage
    getRoute = function
        | HomePage -> "/"
        | StartPage -> "/start"
        | InvitePage -> "/invite"
        | JoinPage id -> sprintf "/join/%O" id
        | ConfigurePage -> "/configure"
        | PlayPage -> "/play"
        | NotImplementedPage -> "/not-implemented"
}
