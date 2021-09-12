[<AutoOpen>]
module Chipper.Web.AppRouter

open System
open Bolero

open Chipper

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
        |> Option.map Message.setPage

    getRoute = function
        | HomePage -> "/"
        | StartPage -> "/start"
        | JoinPage id -> $"/join/{id}"
        | ConfigurePage -> "/configure"
        | PlayPage -> "/play"
        | NotImplementedPage -> "/not-implemented"
}
