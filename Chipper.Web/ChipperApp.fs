module Chipper.Web.ChipperApp

open System
open Microsoft.Extensions.DependencyInjection

open Elmish
open Flurl
open Bolero
open Bolero.Html

open Chipper.Core.Domain
open Chipper.Core.Persistence
open Chipper.Web.Workflows

let init storage repo =
    let model = { Page = HomePage; State = NoState; LocalState = None; IsLoaded = false }
    let cmd = Cmd.OfAsync.perform (fun () -> getState storage repo) () LoadLocalState

    model, cmd

let update storage repo message model =
    match message, model.State with

    | SetError e, _ ->
        printfn "An unhandled error appeared: %O" e
        model, Cmd.none

    | SetPage (JoinPage id as page), _ ->
        { model with Page = page }, Cmd.OfAsync.result (repo |> getSessionToJoin id)

    | SetPage page, _ ->
        { model with Page = page }, Cmd.none

    | LoadLocalState state, _ ->
        loadState model state

    | RecoverLocalState, _ ->
        let newModel = { model with LocalState = None }
        match model.LocalState with
        | Some (ConfiguringSession _ as state) ->
            { model with Page = ConfigurePage; State = state; LocalState = None }, Cmd.none
        | _ -> newModel, Cmd.none

    | IgnoreLocalState, _ ->
        { model with LocalState = None }, Cmd.none

    | ClearLocalState, _ ->
        Async.StartImmediate <| storage.ClearState ()
        { model with LocalState = None }, Cmd.none

    | SetModel model, _ ->
        model, Cmd.none
        
    | StartGameSession, ConfiguringSession _ ->
        { model with Page = StartPage }, Cmd.none

    | StartGameSession, _ ->
        { model with Page = StartPage; State = AddingSessionName ("", "") }, Cmd.none

    | InputSessionName name, AddingSessionName (_, playerName) ->
        { model with State = AddingSessionName (name, playerName) }, Cmd.none

    | InputPlayerName playerName, AddingSessionName (name, _) ->
        { model with State = AddingSessionName (name, playerName) }, Cmd.none

    | SaveSessionName, AddingSessionName (name, playerName) ->
        model, Cmd.OfAsync.result <| saveNewSession storage repo model name playerName
        
    | SaveSessionName, ConfiguringSession _ ->
        { model with Page = ConfigurePage }, Cmd.none

    | InputPlayerName name, JoiningSession { GameSessionId = id; GameSessionName = sessionName } ->
        let state = JoiningSession { GameSessionId = id; GameSessionName = sessionName; Name = name }
        { model with State = state }, Cmd.none

    | SetBettingType bettingType, ConfiguringSession config ->
        let config = { config with ConfigBettingType = bettingType }
        model, Cmd.OfAsync.result <| configureSession storage repo model config

    | SetRaiseType raiseType, ConfiguringSession config ->
        let config = { config with ConfigRaiseType = raiseType }
        model, Cmd.OfAsync.result <| configureSession storage repo model config

    | _ ->
        model, Cmd.none

let mainView js createJoinUrl model dispatch =
    match model with
    | { IsLoaded = false } ->
        Empty

    | { Page = HomePage } ->
        View.homePage dispatch

    | { Page = StartPage; State = AddingSessionName (sessionName, playerName) } ->
        let isValid = Model.canSaveSessionName sessionName && Model.canSavePlayerName playerName
        View.startPage isValid false sessionName playerName dispatch

    | { Page = JoinPage _; State = JoiningSession ({ GameSessionName = (GameSessionName name) } as player) } ->
        View.joinPage name (player |> Model.tryCreateJoinInfo) dispatch

    | { Page = JoinPage _; State = JoiningInvalidSession } ->
        View.invalidJoinPage

    | {
        Page = StartPage
        State = ConfiguringSession {
            ConfigName = GameSessionName sessionName
            ConfigHost = { Name = PlayerName hostName }
        }
     } ->
        View.startPage true true sessionName hostName dispatch

    | { Page = ConfigurePage; State = ConfiguringSession config } ->
        View.configurePage js config (createJoinUrl config.ConfigId) dispatch

    | _ -> View.notImplementedPage

let view js createJoinUrl model dispatch =
    concat [
        mainView js createJoinUrl model dispatch

        match model.LocalState with
        | Some state -> ToastComponent.localState state dispatch
        | _ -> empty
    ]

type AppComponent() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let settings = this.Services.GetRequiredService<AppSettings>()
        let repo = this.Services.GetRequiredService<GameSessionRepository>()
        let storage = this.Services.GetRequiredService<LocalStorage>()

        let createJoinUrl = fun (GameSessionId id) -> Url.Combine(settings.UrlRoot, (router.Link <| JoinPage id))

        let init _ = init storage repo
        let update = update storage repo
        let view = view this.JSRuntime createJoinUrl

        Program.mkProgram init update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withConsoleTrace
#endif
