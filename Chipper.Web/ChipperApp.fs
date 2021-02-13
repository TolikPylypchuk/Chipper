module Chipper.Web.ChipperApp

open Elmish

open Bolero
open Bolero.Html

let router = Router.infer SetPage (fun m -> m.Page)

let init _ = { Page = StartPage }, Cmd.none

let update message model =
    match message with
    | SetPage page -> { model with Page = page }, Cmd.none
    | CreateSession -> { Page = NotImplemented }, Cmd.none

let render model dispatch =
    match model.Page with
    | StartPage -> View.startPage dispatch
    | _ -> View.notImplemented

type AppComponent() =
    inherit ProgramComponent<Model, Message>()

    override _.Program =
        Program.mkProgram init update render
        |> Program.withRouter router
