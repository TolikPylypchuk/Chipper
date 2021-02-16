module Chipper.Web.ChipperApp

open Microsoft.Extensions.DependencyInjection

open FSharpPlus

open Blazored.LocalStorage
open Elmish
open Flurl
open Bolero

open Chipper.Core
open Chipper.Core.Domain
open Chipper.Core.Persistence

let getFullState storage repo = async {
    let! localState = storage.GetState ()
    let! currentState = async {
        match localState with
        | StartingSession newSession ->
            match! newSession.Id |> repo.GetSession with
            | Ok (NewSession newSession) -> return StartingSession newSession |> Some
            | _ -> return None
        | _ -> return localState |> Some
    }

    match currentState with
    | Some state ->
        if state <> localState then
            do! storage.SetState state
        return state
    | None ->
        do! storage.ClearState ()
        return NoState
}

let init storage repo =
    let model = { Page = HomePage; State = NotLoaded }
    let cmd = Cmd.OfAsync.either (fun () -> getFullState storage repo) () SetInitialState SetError

    model, cmd

let startNewSession storage repo model =
    model, Cmd.OfAsync.result <| (async {
        match GameSessionName.create "test" with
        | Ok name ->
            match! repo.CreateSession name with
            | Ok newSession ->
                let state = StartingSession newSession
                do! storage.SetState state
                return { model with Page = StartPage; State = state }
            | _ -> return model
        | _ -> return model
    } |> Async.map SetModel)

let update storage repo message model =
    match message, model.State with
    | SetInitialState state, _ -> { model with State = state }, Cmd.none
    | SetPage page, _ -> { model with Page = page }, Cmd.none
    | SetModel model, _ -> model, Cmd.none
    | _, NotLoaded -> model, Cmd.none
    | SetError e, _ ->
        printfn "%O" e.Message
        model, Cmd.none
    | AddSessionName _, _ -> model, Cmd.none
    | StartGameSession, StartingSession _ ->
        { model with Page = StartPage }, Cmd.none
    | StartGameSession, _ ->
        startNewSession storage repo model
    | ConfigureGameSession, _ ->
        model, Cmd.none

let view createJoinUrl model dispatch =
    match model.Page, model.State with
    | _, NotLoaded -> Empty
    | HomePage, _ -> View.homePage dispatch
    | StartPage, StartingSession newSession -> View.startPage (createJoinUrl newSession.Id) dispatch
    | _ -> View.notImplementedPage

type AppComponent() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let settings = this.Services.GetRequiredService<AppSettings>()
        let repo = this.Services.GetRequiredService<GameSessionRepository>()
        let localStorage = this.Services.GetRequiredService<ILocalStorageService>()

        let storage = {
            GetState = fun () -> LocalStorage.getLocalState localStorage
            SetState = LocalStorage.setLocalState localStorage
            ClearState = fun () -> LocalStorage.clearLocalState localStorage
        }

        let createJoinUrl = fun (GameSessionId id) -> Url.Combine(settings.UrlRoot, (router.Link <| JoinPage id))

        let init _ = init storage repo
        let update = update storage repo
        let view = view createJoinUrl

        Program.mkProgram init update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withConsoleTrace
#endif
