module Chipper.Web.ChipperApp

open Microsoft.Extensions.DependencyInjection

open Elmish
open Blazored.LocalStorage
open Bolero

open Chipper.Core.Persistence

let init getState =
    let model = { Page = HomePage; State = NotLoaded }
    let cmd = Cmd.OfAsync.either getState () SetInitialState SetError

    model, cmd

let startNewSession newId setState model =
    let id = newId ()
    let state = StartingSession <| id
    Async.StartImmediate <| setState state
    { model with Page = StartPage; State = state }, Cmd.none

let update newId setState message model =
    match message, model.State with
    | SetInitialState state, _ -> { model with State = state }, Cmd.none
    | SetPage page, _ -> { model with Page = page }, Cmd.none
    | _, NotLoaded -> model, Cmd.none
    | SetError e, _ ->
        printfn "%O" e.Message
        model, Cmd.none
    | StartGameSession, StartingSession _ ->
        { model with Page = StartPage }, Cmd.none
    | StartGameSession, _ ->
        startNewSession newId setState model
    | ConfigureGameSession, _ ->
        model, Cmd.none

let view settings model dispatch =
    match model.Page, model.State with
    | _, NotLoaded -> Empty
    | HomePage, _ -> View.homePage dispatch
    | StartPage, StartingSession id -> View.startPage settings id dispatch
    | _ -> View.notImplementedPage

type AppComponent() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let settings = this.Services.GetRequiredService<AppSettings>()
        let repo = this.Services.GetRequiredService<GameSessionRepository>()
        let localStorage = this.Services.GetRequiredService<ILocalStorageService>()

        let getState = LocalStorage.getLocalState localStorage
        let setState = LocalStorage.setLocalState localStorage

        let update = update repo.CreateId setState
        let view = view settings

        Program.mkProgram (fun _ -> init getState) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withConsoleTrace
#endif
