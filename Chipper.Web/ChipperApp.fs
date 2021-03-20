module Chipper.Web.ChipperApp

open Microsoft.Extensions.DependencyInjection

open FSharpPlus
open FSharpPlus.Data

open Elmish

open Flurl

open Bolero
open Bolero.Html

open Chipper.Core.Domain
open Chipper.Core.Persistence

let init = monad {
    let! env = Reader.ask

    let model = { Page = HomePage; State = NoState; LocalState = None; IsLoaded = false }
    let cmd = Cmd.OfAsync.perform (Reader.run Flow.getState) env Message.loadLocalState

    return model, cmd
}

let updateGeneric message model =
    match message, model.State with
    | SetError e, _ ->
        model |> Flow.setError e |> Env.none
    | SetPage (JoinPage id as page), (AwaitingJoinConfirmation _ | AwaitingGameStart _)  ->
        model |> Flow.setPage page |> Env.none
    | SetPage (JoinPage id as page), _  ->
        model |> Flow.setJoinPage id page
    | SetPage page, _  ->
        model |> Flow.setPage page |> Env.none
    | LoadLocalState state, _  ->
        model |> Flow.loadState state
    | RecoverLocalState, _  ->
        model |> Flow.recoverLocalState
    | IgnoreLocalState, _  ->
        model |> Flow.ignoreLocalState |> Env.none
    | ClearLocalState, _  ->
        model |> Flow.clearLocalState
    | SetModel model, _  ->
        model |> Flow.doNothing |> Env.none
    | ReceiveEvent event, _  ->
        model |> EventFlow.receiveEvent event

let updateGameStart message model =
    match message, model.State with
    | StartGameSession, ConfiguringSession _ ->
        model |> GameStartFlow.startSessionWhenConfiguring |> Env.none

    | StartGameSession, _ ->
        model |> GameStartFlow.startSession |> Env.none

    | InputSessionName name, AddingSessionName (_, playerName) ->
        model |> GameStartFlow.inputSessionName name playerName |> Env.none

    | InputPlayerName playerName, AddingSessionName (name, _) ->
        model |> GameStartFlow.inputPlayerNameWhenAddingSessionName name playerName |> Env.none

    | SaveSessionName, AddingSessionName (name, playerName) ->
        model |> GameStartFlow.saveSessionName name playerName

    | SaveSessionName, ConfiguringSession _ ->
        model |> GameStartFlow.saveSessionNameWhenConfiguring |> Env.none

    | SessionSaved config, _ ->
        model |> GameStartFlow.onSessionSaved config

    | InputPlayerName name, JoiningSession { GameSessionId = id; GameSessionName = sessionName } ->
        model |> GameStartFlow.inputPlayerNameWhenJoiningSession id sessionName name |> Env.none

    | RequestAccess joinInfo, JoiningSession player ->
        model |> GameStartFlow.requestAccess player joinInfo

    | RequestAccess joinInfo, (AwaitingJoinRejected player | AwaitingGameStartRemoved player) ->
        model |> GameStartFlow.requestAccessAgain player joinInfo

    | AcceptRename, AwaitingGameStartRenamed (player, _) ->
        model |> GameStartFlow.acceptRename player |> Env.none

    | _ ->
        model |> Flow.doNothing |> Env.none

let updateConfig message model =
    match message, model.State with
    | SetBettingType bettingType, ConfiguringSession state ->
        model |> ConfigFlow.setBettingType bettingType state

    | SetRaiseType raiseType, ConfiguringSession state ->
        model |> ConfigFlow.setRaiseType raiseType state
        
    | EditPlayerName playerName, ConfiguringSession state ->
        model |> ConfigFlow.editPlayerName playerName state |> Env.none

    | AcceptPlayerRequest playerName, ConfiguringSession state ->
        model |> ConfigFlow.acceptPlayerRequest playerName state

    | RejectPlayerRequest playerName, ConfiguringSession state ->
        model |> ConfigFlow.rejectPlayerRequest playerName state

    | ConfigInputPlayerName editedName, ConfiguringSession ({ EditMode = Player (playerName, _) } as state) ->
        model |> ConfigFlow.inputPlayerName playerName editedName state |> Env.none

    | AcceptPlayerNameEdit, ConfiguringSession ({ EditMode = Player (playerName, editedName) } as state) ->
        model |> ConfigFlow.acceptPlayerNameEdit playerName editedName state

    | CancelPlayerNameEdit, ConfiguringSession state ->
        model |> ConfigFlow.cancelPlayerNameEdit state |> Env.none
        
    | RemovePlayer playerName, ConfiguringSession state ->
        model |> ConfigFlow.removePlayer playerName state

    | _ ->
        model |> Flow.doNothing |> Env.none

let update message model =
    match message with
    | GenericMessage message -> updateGeneric message model
    | GameStartMessage message -> updateGameStart message model
    | ConfigMessage message -> updateConfig message model

let mainView js createJoinUrl model dispatch =
    match model.Page, model.State with
    | HomePage, _ ->
        GameStartView.homePage dispatch

    | StartPage, AddingSessionName (sessionName, playerName) ->
        let isValid = Model.canSaveSessionName sessionName && Model.canSavePlayerName playerName
        GameStartView.startPage isValid false sessionName playerName dispatch
        
    | JoinPage _, NoState ->
        empty

    | JoinPage _, JoiningSession player ->
        GameStartView.joinPage player.Name player.GameSessionName (player |> Model.tryCreateJoinInfo) dispatch

    | JoinPage _, AwaitingJoinConfirmation player ->
        GameStartView.awaitJoinPage player.ValidGameSessionName

    | JoinPage _, AwaitingGameStart player ->
        GameStartView.lobbyPage player.ValidGameSessionName None dispatch

    | JoinPage _, AwaitingGameStartRenamed (player, renameInfo) ->
        GameStartView.lobbyPage player.ValidGameSessionName (Some renameInfo) dispatch

    | JoinPage _, AwaitingJoinRejected player ->
        GameStartView.rejectedJoinPage player.ValidGameSessionName (player |> Model.createJoinInfo) false dispatch

    | JoinPage _, AwaitingGameStartRemoved player ->
        GameStartView.rejectedJoinPage player.ValidGameSessionName (player |> Model.createJoinInfo) true dispatch

    | JoinPage _, JoiningInvalidSession ->
        GameStartView.invalidJoinPage

    | StartPage,
      ConfiguringSession { Config = { ConfigName = GameSessionName sessionName
                                      ConfigHost = { Name = PlayerName hostName } } } ->
        GameStartView.startPage true true sessionName hostName dispatch

    | ConfigurePage, ConfiguringSession state ->
        let joinUrl = createJoinUrl state.Config.ConfigId
        let isNameValid = ConfigFlow.isEditedPlayerNameValid state.Config.ConfigPlayers
        ConfigView.configurePage js state joinUrl isNameValid dispatch

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

        let init _ = Reader.run init env
        let update = fun message model -> Reader.run (update message model) env
        let view = view this.JSRuntime createJoinUrl

        Program.mkProgram init update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withConsoleTrace
#endif
