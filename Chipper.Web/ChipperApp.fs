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
    match message with
    | SetError e -> model |> Flow.setError e |> Env.none
    | SetPage (JoinPage id as page) -> model |> Flow.setJoinPage id page
    | SetPage page -> model |> Flow.setPage page |> Env.none
    | LoadLocalState state -> model |> Flow.loadState state |> Env.none
    | RecoverLocalState -> model |> Flow.recoverLocalState
    | IgnoreLocalState -> model |> Flow.ignoreLocalState |> Env.none
    | ClearLocalState -> model |> Flow.clearLocalState
    | SetModel model -> model |> Flow.doNothing |> Env.none
    | ReceiveEvent event -> model |> Flow.receiveEvent event |> Env.none

let updateGameStart message model =
    match message, model.State with
    | StartGameSession, ConfiguringSession _ ->
        model |> GameStartFlow.startSessionWhenConfiguring |> Env.none

    | StartGameSession, _ ->
        model |> GameStartFlow.startSession |> Env.none

    | InputSessionName name, AddingSessionName (_, playerName) ->
        model |> GameStartFlow.inputSessionName name playerName |> Env.none

    | SaveSessionName, AddingSessionName (name, playerName) ->
        model |> GameStartFlow.saveSessionName name playerName
        
    | SaveSessionName, ConfiguringSession _ ->
        model |> GameStartFlow.saveSessionNameWhenConfiguring |> Env.none

    | SessionSaved config, _ ->
        model |> GameStartFlow.onSessionSaved config
        
    | _ ->
        model |> Flow.doNothing |> Env.none

let updatePlayer message model =
    match message, model.State with
    | InputPlayerName playerName, AddingSessionName (name, _) ->
        model |> PlayerFlow.inputPlayerNameWhenAddingSessionName name playerName |> Env.none

    | InputPlayerName name, JoiningSession { GameSessionId = id; GameSessionName = sessionName } ->
        model |> PlayerFlow.inputPlayerNameWhenJoiningSession id sessionName name |> Env.none

    | RequestAccess joinInfo, JoiningSession player ->
        model |> PlayerFlow.requestAccess player joinInfo

    | RequestAccess joinInfo, AwaitingJoinRejected player ->
        model |> PlayerFlow.requestAccessAgain player joinInfo

    | EditPlayerName playerName, ConfiguringSession state ->
        model |> PlayerFlow.editPlayerName playerName state |> Env.none

    | AcceptPlayerRequest playerName, ConfiguringSession state ->
        model |> PlayerFlow.acceptPlayerRequest playerName state

    | RejectPlayerRequest playerName, ConfiguringSession state ->
        model |> PlayerFlow.rejectPlayerRequest playerName state

    | InputPlayerName editedName, ConfiguringSession ({ EditMode = Player (playerName, _) } as state) ->
        model |> PlayerFlow.inputNameWhenConfiguringSession playerName editedName state |> Env.none

    | AcceptPlayerNameEdit, ConfiguringSession ({ EditMode = Player (playerName, editedName) } as state) ->
        model |> PlayerFlow.acceptPlayerNameEdit playerName editedName state

    | CancelPlayerNameEdit, ConfiguringSession state ->
        model |> PlayerFlow.cancelPlayerNameEdit state |> Env.none

    | _ ->
        model |> Flow.doNothing |> Env.none

let updateConfig message model =
    match message, model.State with
    | SetBettingType bettingType, ConfiguringSession { Config = config } ->
        model |> ConfigFlow.setBettingType bettingType config

    | SetRaiseType raiseType, ConfiguringSession { Config = config } ->
        model |> ConfigFlow.setRaiseType raiseType config

    | _ ->
        model |> Flow.doNothing |> Env.none

let update message model =
    match message with
    | GenericMessage message -> updateGeneric message model
    | GameStartMessage message -> updateGameStart message model
    | PlayerMessage message -> updatePlayer message model
    | ConfigMessage message -> updateConfig message model

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
        let isNameValid = PlayerFlow.isEditedPlayerNameValid state.Config.ConfigPlayers
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

        let env = { Storage = storage; Repo = repo; Mediator = mediator }

        let init _ = Reader.run init env
        let update = fun message model -> Reader.run (update message model) env
        let view = view this.JSRuntime createJoinUrl

        Program.mkProgram init update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withConsoleTrace
#endif
