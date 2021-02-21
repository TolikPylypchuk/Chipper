module Chipper.Web.ChipperApp

open System
open Microsoft.Extensions.DependencyInjection

open FSharp.Control.Reactive
open FSharpx.Collections

open Elmish
open Flurl
open Bolero

open Chipper.Core.Domain
open Chipper.Core.Persistence
open Chipper.Web.Workflows

let private inputSubject = Subject.behavior ""

let private inputDebouncer = inputSubject |> Observable.throttle (TimeSpan.FromMilliseconds(300.0))

let cmdDebounceInput message x =
    Cmd.ofSub (fun dispatch ->
        inputSubject
        |> Subject.onNext x
        |> ignore

        inputDebouncer
        |> Observable.take 1
        |> Observable.map DebounceEnd
        |> Observable.map message
        |> Observable.subscribe dispatch
        |> ignore)

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
        | Some (StartingSession _ as state) ->
            { model with Page = InvitePage; State = state; LocalState = None }, Cmd.none
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

    | StartGameSession, _ ->
        { model with Page = StartPage; State = AddingSessionName ("", "") }, Cmd.none

    | InputSessionName (DebounceStart name), _ ->
        model, name |> cmdDebounceInput InputSessionName

    | InputSessionName (DebounceEnd name), AddingSessionName (_, playerName) ->
        { model with State = AddingSessionName (name, playerName) }, Cmd.none

    | InputPlayerName (DebounceStart name), _ ->
        model, name |> cmdDebounceInput InputPlayerName

    | InputPlayerName (DebounceEnd playerName), AddingSessionName (name, _) ->
        { model with State = AddingSessionName (name, playerName) }, Cmd.none

    | SaveSessionName, AddingSessionName (name, playerName) ->
        model, Cmd.OfAsync.result <| startNewSession storage repo model name playerName

    | InputPlayerName (DebounceEnd name), JoiningSession { GameSessionId = id; GameSessionName = sessionName } ->
        let state = JoiningSession { GameSessionId = id; GameSessionName = sessionName; Name = name }
        { model with State = state }, Cmd.none

    | ConfigureGameSession, StartingSession newSession ->
        let config = newSession |> GameSession.defaultConfig
        model, Cmd.OfAsync.result <| configureSession storage repo model config
        
    | ConfigureGameSession, ConfiguringSession _ ->
        { model with Page = ConfigurePage }, Cmd.none

    | SetBettingType bettingType, ConfiguringSession config ->
        let config = { config with BettingType = bettingType }
        model, Cmd.OfAsync.result <| configureSession storage repo model config

    | SetRaiseType raiseType, ConfiguringSession config ->
        let config = { config with RaiseType = raiseType }
        model, Cmd.OfAsync.result <| configureSession storage repo model config

    | _ ->
        model, Cmd.none

let view js createJoinUrl model dispatch =
    match model with
    | { IsLoaded = false } ->
        Empty

    | { Page = HomePage } ->
        View.homePage model dispatch

    | { Page = StartPage; State = AddingSessionName (name, playerName) } ->
        let isValid = Model.canSaveSessionName name && Model.canSavePlayerName playerName
        View.startPage isValid model dispatch

    | { Page = InvitePage; State = StartingSession newSession } ->
        View.invitePage js (createJoinUrl newSession.Id) model dispatch
        
    | { Page = InvitePage; State = ConfiguringSession config } ->
        View.invitePage js (createJoinUrl config.Id) model dispatch

    | { Page = JoinPage _; State = JoiningSession ({ GameSessionName = (GameSessionName name) } as player) } ->
        View.joinPage name (player |> Model.tryCreateJoinInfo) model dispatch

    | { Page = JoinPage _; State = JoiningInvalidSession } ->
        View.invalidJoinPage model dispatch

    | { Page = ConfigurePage; State = ConfiguringSession config } ->
        View.configurePage config dispatch

    | _ -> View.notImplementedPage

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
