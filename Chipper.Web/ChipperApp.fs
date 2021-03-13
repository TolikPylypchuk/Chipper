module Chipper.Web.ChipperApp

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

let update storage repo mediator message model =
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
        | Some (ConfiguringSession { Config = { ConfigId = id } } as state) ->
            { model with Page = ConfigurePage; State = state; LocalState = None }, mediator |> createEventLoop id
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
        model, Cmd.OfAsync.result <| saveNewSession repo name playerName

    | SessionSaved config, _ ->
        let state = ConfiguringSession { Config = config; PlayerRequests = []; EditMode = NoEdit }
        storage |> setStateSimple state
        { model with Page = ConfigurePage; State = state }, mediator |> createEventLoop config.ConfigId

    | SaveSessionName, ConfiguringSession _ ->
        { model with Page = ConfigurePage }, Cmd.none

    | InputPlayerName name, JoiningSession { GameSessionId = id; GameSessionName = sessionName } ->
        let state = JoiningSession { GameSessionId = id; GameSessionName = sessionName; Name = name }
        { model with State = state }, Cmd.none

    | RequestAccess joinInfo, JoiningSession player ->
        let validPlayer = player |> createValidPlayer joinInfo
        let newState = requestAccess mediator joinInfo validPlayer
        { model with State = newState }, mediator |> createEventLoop player.GameSessionId

    | RequestAccess joinInfo, AwaitingJoinRejected player ->
        { model with State = requestAccess mediator joinInfo player }, Cmd.none

    | ReceiveEvent (PlayerAccessRequested joinInfo), ConfiguringSession state ->
        let newState = { state with PlayerRequests = state.PlayerRequests @ [ joinInfo ] }
        { model with State = ConfiguringSession newState }, Cmd.none
        
    | ReceiveEvent (PlayerAccepted playerName), AwaitingJoinConfirmation player when player.ValidName = playerName ->
        { model with State = AwaitingGameStart player }, Cmd.none
        
    | ReceiveEvent (PlayerRejected playerName), AwaitingJoinConfirmation player when player.ValidName = playerName ->
        { model with State = AwaitingJoinRejected player }, Cmd.none

    | SetBettingType bettingType, ConfiguringSession { Config = config } ->
        let config = { config with ConfigBettingType = bettingType }
        model, Cmd.OfAsync.result <| configureSession storage repo model config

    | SetRaiseType raiseType, ConfiguringSession { Config = config } ->
        let config = { config with ConfigRaiseType = raiseType }
        model, Cmd.OfAsync.result <| configureSession storage repo model config

    | EditPlayerName playerName, ConfiguringSession state ->
        let newState =
            { state with
                EditMode = ConfigSessionEditMode.Player (playerName, playerName |> PlayerName.value)
            }

        { model with State = ConfiguringSession newState }, Cmd.none

    | AcceptPlayerRequest playerName, ConfiguringSession state ->
        { model with State = acceptPlayerRequest mediator state playerName }, Cmd.none

    | RejectPlayerRequest playerName, ConfiguringSession state ->
        { model with State = rejectPlayerRequest mediator state playerName }, Cmd.none

    | InputPlayerName editedName, ConfiguringSession ({ EditMode = Player (playerName, _) } as state) ->
        { model with State = ConfiguringSession { state with EditMode = Player (playerName, editedName) } }, Cmd.none

    | AcceptEdit, ConfiguringSession ({ EditMode = Player (playerName, editedName) } as state) ->
        { model with State = editPlayerName mediator state playerName editedName }, Cmd.none

    | CancelEdit, ConfiguringSession state ->
        { model with State = ConfiguringSession { state with EditMode = NoEdit } }, Cmd.none

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

    | { Page = JoinPage _; State = JoiningSession player } ->
        View.joinPage player.GameSessionName (player |> Model.tryCreateJoinInfo) dispatch

    | { Page = JoinPage _; State = AwaitingJoinConfirmation player } ->
        View.awaitJoinPage player.ValidGameSessionName
        
    | { Page = JoinPage _; State = AwaitingGameStart player } ->
        View.lobbyPage player.ValidGameSessionName
        
    | { Page = JoinPage _; State = AwaitingJoinRejected player } ->
        View.rejectedJoinPage player.ValidGameSessionName (player |> Model.createJoinInfo) dispatch

    | { Page = JoinPage _; State = JoiningInvalidSession } ->
        View.invalidJoinPage

    | {
        Page = StartPage
        State = ConfiguringSession
            { Config = { ConfigName = GameSessionName sessionName; ConfigHost = { Name = PlayerName hostName } } }
     } ->
        View.startPage true true sessionName hostName dispatch

    | { Page = ConfigurePage; State = ConfiguringSession state } ->
        let joinUrl = createJoinUrl state.Config.ConfigId
        let isNameValid = isEditedPlayerNameValid state.Config.ConfigPlayers
        View.configurePage js state joinUrl isNameValid dispatch

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
        let mediator = this.Services.GetRequiredService<IEventMediator>()

        let createJoinUrl = fun (GameSessionId id) -> Url.Combine(settings.UrlRoot, (router.Link <| JoinPage id))

        let init _ = init storage repo
        let update = update storage repo mediator
        let view = view this.JSRuntime createJoinUrl

        Program.mkProgram init update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withConsoleTrace
#endif
