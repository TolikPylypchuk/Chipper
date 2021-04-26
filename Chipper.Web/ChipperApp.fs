module Chipper.Web.ChipperApp

open Microsoft.Extensions.DependencyInjection

open FSharpPlus

open Elmish

open Flurl

open Bolero
open Bolero.Html

open Chipper.Core

let init : Flow<Model> = monad {
    let model = { Page = HomePage; State = NoState; LocalState = None; IsLoaded = false }
    do! asyncCmd (Flow.getState |>> Async.map Message.loadLocalState)

    return model
}

let updateGeneric message model =
    match message, model.State with
    | NoMessage, _ ->
        model |> pureFlow
    | SetError e, _ ->
        model |> Flow.setError e
    | SetPage (JoinPage _ as page), (AwaitingJoinConfirmation _ | AwaitingGameStart _)  ->
        model |> Flow.setPage page
    | SetPage (JoinPage id as page), _  ->
        model |> Flow.setJoinPage id page
    | SetPage page, _  ->
        model |> Flow.setPage page
    | LoadLocalState state, _  ->
        model |> Flow.loadState state
    | RecoverLocalState, _  ->
        model |> Flow.recoverLocalState
    | IgnoreLocalState, _  ->
        model |> Flow.ignoreLocalState
    | ClearLocalState, _  ->
        model |> Flow.clearLocalState
    | SetModel model, _  ->
        model |> pureFlow
    | ReceiveEvent event, _  ->
        model |> EventFlow.receiveEvent event
    | CustomMessage (:? Flow<Model> as flow), _ ->
        flow
    | CustomMessage _, _ ->
        model |> pureFlow

let updateGameStart message model =
    match message, model.State with
    | StartGameSession, ConfiguringSession _ ->
        model |> GameStartFlow.startSessionWhenConfiguring

    | StartGameSession, _ ->
        model |> GameStartFlow.startSession

    | InputSessionName sessionName, AddingSessionName { HostName = hostName } ->
        model |> GameStartFlow.inputSessionName sessionName hostName

    | InputPlayerName hostName, AddingSessionName { SessionName = sessionName } ->
        model |> GameStartFlow.inputPlayerNameWhenAddingSessionName sessionName hostName

    | SaveSessionName (sessionName, hostName), AddingSessionName _ ->
        model |> GameStartFlow.saveSessionName sessionName hostName

    | SaveSessionName _, ConfiguringSession _ ->
        model |> GameStartFlow.saveSessionNameWhenConfiguring

    | SessionSaved config, _ ->
        model |> GameStartFlow.onSessionSaved config

    | InputPlayerName playerName, JoiningSession { GameSessionId = id; GameSessionName = sessionName } ->
        model |> GameStartFlow.inputPlayerNameWhenJoiningSession id sessionName playerName

    | RequestAccess joinInfo, JoiningSession player ->
        model |> GameStartFlow.requestAccess player joinInfo

    | RequestAccessAgain request, (AwaitingJoinRejected player | AwaitingGameStartRemoved player) ->
        model |> GameStartFlow.requestAccessAgain player request

    | AcceptRename, AwaitingGameStartRenamed (player, _) ->
        model |> GameStartFlow.acceptRename player

    | CancelRequest,
      (AwaitingGameStart player | AwaitingJoinConfirmation player | AwaitingGameStartRenamed (player, _)) ->
        model |> GameStartFlow.cancelRequest player

    | _ ->
        model |> pureFlow

let updateConfig message model =
    match message, model.State with
    | AcceptPlayerRequest playerName, ConfiguringSession state ->
        model |> ConfigFlow.acceptPlayerRequest playerName state

    | RejectPlayerRequest playerName, ConfiguringSession state ->
        model |> ConfigFlow.rejectPlayerRequest playerName state

    | EditSessionName, ConfiguringSession state ->
        model |> ConfigFlow.editSessionName state

    | ConfigInputSessionName editedName, ConfiguringSession ({ EditMode = EditSession _ } as state) ->
        model |> ConfigFlow.inputSessionName editedName state

    | EditPlayerName playerId, ConfiguringSession state ->
        model |> ConfigFlow.editPlayerName playerId state

    | ConfigInputPlayerName editedName, ConfiguringSession ({ EditMode = EditPlayer { Id = playerId } } as state) ->
        model |> ConfigFlow.inputPlayerName playerId editedName state
        
    | AcceptSessionNameEdit newName, ConfiguringSession ({ EditMode = EditSession _ } as state) ->
        model |> ConfigFlow.acceptSessionNameEdit newName state

    | AcceptPlayerNameEdit newPlayers, ConfiguringSession ({ EditMode = EditPlayer { Id = playerId } } as state) ->
        model |> ConfigFlow.acceptPlayerNameEdit playerId newPlayers state

    | CancelEdit, ConfiguringSession state ->
        model |> ConfigFlow.cancelEdit state

    | RemovePlayer playerId, ConfiguringSession state ->
        model |> ConfigFlow.removePlayer playerId state

    | MovePlayerUp playerId, ConfiguringSession state ->
        model |> ConfigFlow.movePlayer PlayerList.movePlayerUp playerId state

    | MovePlayerDown playerId, ConfiguringSession state ->
        model |> ConfigFlow.movePlayer PlayerList.movePlayerDown playerId state

    | SetChipEqualDistributionValue (chip, value),
      ConfiguringSession ({ Config = { ConfigChipDistribution = EqualChipDitribution chips } } as state) ->
        model |> ConfigFlow.setChipEqualDistributionValue chip value chips state
        
    | InputBetRoundNumber num, ConfiguringSession state ->
        model |> ConfigFlow.inputBetRoundNumber num state

    | _ ->
        model |> pureFlow

let update message model =
    match message with
    | GenericMessage message -> updateGeneric message model
    | GameStartMessage message -> updateGameStart message model
    | ConfigMessage message -> updateConfig message model

let mainView js createJoinUrl model dispatch =
    match model.Page, model.State with
    | HomePage, _ ->
        GameStartView.homePage dispatch

    | StartPage, AddingSessionName state ->
        GameStartView.startPage false state dispatch
        
    | JoinPage _, NoState ->
        empty

    | JoinPage _, JoiningSession player ->
        GameStartView.joinPage player dispatch

    | JoinPage _, AwaitingJoinConfirmation player ->
        GameStartView.awaitJoinPage player.ValidGameSessionName dispatch

    | JoinPage _, AwaitingGameStart player ->
        GameStartView.lobbyPage player.ValidGameSessionName None dispatch

    | JoinPage _, AwaitingGameStartRenamed (player, renameInfo) ->
        GameStartView.lobbyPage player.ValidGameSessionName (Some renameInfo) dispatch

    | JoinPage _, AwaitingJoinRejected player ->
        let request = player |> Model.createJoinRequest
        GameStartView.rejectedJoinPage player.ValidGameSessionName request false dispatch

    | JoinPage _, AwaitingGameStartRemoved player ->
        let request = player |> Model.createJoinRequest
        GameStartView.rejectedJoinPage player.ValidGameSessionName request true dispatch
        
    | JoinPage _, JoinRequestCanceled sessionName ->
        GameStartView.joinRequestCanceledPage sessionName

    | JoinPage _, JoiningInvalidSession ->
        GameStartView.invalidJoinPage

    | StartPage, ConfiguringSession { Config = config } ->
        let state = config |> Model.addSessionStateFromConfig
        GameStartView.startPage true state dispatch

    | ConfigurePage, ConfiguringSession state ->
        let joinUrl = createJoinUrl state.Config.ConfigId
        ConfigView.configPage js state joinUrl dispatch

    | _ ->
        View.notImplementedPage

let view js createJoinUrl model dispatch =
    if model.IsLoaded then
        concat [
            mainView js createJoinUrl model dispatch

            match model.LocalState with
            | Some state -> ToastComponent.localState state dispatch
            | _ -> empty
        ]
    else
        empty

type AppComponent() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let settings = this.Services.GetRequiredService<AppSettings>()
        let repo = this.Services.GetRequiredService<GameSessionRepository>()
        let storage = this.Services.GetRequiredService<LocalStorage>()
        let mediator = this.Services.GetRequiredService<IEventMediator>()

        let createJoinUrl = fun (GameSessionId id) -> Url.Combine(settings.UrlRoot, (router.Link <| JoinPage id))

        let env = { Storage = storage; Repo = repo; Mediator = mediator }

        let init _ = init |> Flow.run env
        let update = fun message model -> update message model |> Flow.run env
        let view = view this.JSRuntime createJoinUrl

        Program.mkProgram init update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withConsoleTrace
#endif
