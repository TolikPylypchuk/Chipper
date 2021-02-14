module Chipper.Web.ChipperApp

open System
open Microsoft.Extensions.DependencyInjection

open Elmish
open Bolero

open Chipper.Core

let init _ = { Page = HomePage; State = NoState }, Cmd.none

let update newId message model =
    match message with
    | SetPage page -> { model with Page = page }, Cmd.none
    | CreateSession ->
        let id = newId ()
        { model with Page = StartPage id }, Cmd.none

let render settings model dispatch =
    match model.Page with
    | HomePage -> View.homePage dispatch
    | StartPage id -> View.startPage settings (GameSessionId id) dispatch
    | _ -> View.notImplementedPage

type AppComponent() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let settings = this.Services.GetRequiredService<AppSettings>()
        Program.mkProgram init (update Guid.NewGuid) (render settings)
        |> Program.withRouter router
