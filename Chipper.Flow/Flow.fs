module Chipper.Flow

open FSharpPlus
open FSharpPlus.Data

open FsToolkit.ErrorHandling
open Elmish

let run env flow =
    ReaderT.run flow env
    |> Writer.run
    |> Tuple2.mapItem2 (function [] -> Cmd.none | [ cmd ] -> cmd | cmds -> Cmd.batch cmds)

let setPage page model =
    { model with Page = page } |> pureFlow

let setError e model : Flow<Model> =
    printfn "An unhandled error appeared: %O" e
    model |> pureFlow

let private getConfigSessionState repo config = async {
    match! repo |> Persistence.getSession config.ConfigId with
    | Ok (ConfigurableSession config) ->
        return ConfiguringSession { Config = config; EditMode = NoEdit; Target = GameSession.fromConfig config } |> Some
    | _ ->
        return None
}

let private getJoinConfirmationState repo player = async {
    match! repo |> Persistence.getSession player.ValidGameSessionId with
    | Ok (ConfigurableSession config) ->
        let player =
            { player with
                ValidGameSessionId = config.ConfigId
                ValidGameSessionName = config.ConfigName
            }

        let localState =
            if config.ConfigPlayers
                |> PlayerList.configPlayers
                |> List.exists (fun player' -> player'.Id = player.ValidId)
            then AwaitingGameSessionStart player
            else AwaitingJoinConfirmation player

        return Some localState
    | _ ->
        return None
}

let private getAwaitingGameSessionStartState repo player = async {
    match! repo |> Persistence.getSession player.ValidGameSessionId with
    | Ok (ConfigurableSession config) ->
        let player =
            { player with
                ValidGameSessionId = config.ConfigId
                ValidGameSessionName = config.ConfigName
            }

        return Some <| AwaitingGameSessionStart player
    | _ ->
        return None
}

let private getAwaitingGameStartState repo (gameSessionState : GameSessionState) = async {
    match! repo |> Persistence.getSession (gameSessionState.GameSession |> GameSession.id) with
    | Ok (PersistentSession gameSession) ->
        return Some <| AwaitingGameStart { GameSession = gameSession; Player = gameSessionState.Player }
    | _ ->
        return None
}

let private getPlayingState repo gameState = async {
    match! repo |> Persistence.getSession (gameState.GameSession |> GameSession.id) with
    | Ok (PersistentSession gameSession) ->
        match gameSession |> GameSession.currentGame with
        | Some game when gameState.Player.Id = game.CurrentPlayer.Id ->
            return Some <| Betting { GameState = { Game = game; GameSession = gameSession; Player = gameState.Player } }
        | Some game ->
            return Some <| AwaitingTurn { Game = game; GameSession = gameSession; Player = gameState.Player }
        | None ->
            return None
    | _ ->
        return None
}

let getState : Flow<Async<LocalState>> = monad {
    let! storage = Env.askStorage
    let! repo = Env.askRepo

    return async {
        let! localState = storage |> LocalStorage.getState
        let! currentState =
            match localState with
            | ConfiguringSession { Config = config } -> config |> getConfigSessionState repo
            | AwaitingJoinConfirmation player -> player |> getJoinConfirmationState repo
            | AwaitingGameSessionStart player -> player |> getAwaitingGameSessionStartState repo
            | AwaitingGameStart gameSessionState -> gameSessionState |> getAwaitingGameStartState repo
            | AwaitingTurn gameState | Betting { GameState = gameState } -> gameState |> getPlayingState repo
            | _ -> async.Return None

        match currentState with
        | Some state ->
            if state <> localState then
                do! storage |> LocalStorage.setState state
            return state
        | None ->
            do! storage |> LocalStorage.clearState
            return NoState
    }
}

let setStateSimple state : Flow<unit> = monad {
    let! storage = Env.askStorage
    do! cmd <| Cmd.OfAsync.perform (LocalStorage.setState state) storage withNoMessage
}

let clearStateSimple : Flow<unit> = monad {
    let! storage = Env.askStorage
    do! cmd <| Cmd.OfAsync.perform LocalStorage.clearState storage withNoMessage
}

let getSessionToJoin id : Flow<Async<Message>> = monad {
    let! repo = Env.askRepo
    let page = JoinPage id
    let id = GameSessionId id

    let result = asyncResult {
        let! session = repo |> Persistence.getSession id

        let! sessionName =
            match session with
            | ConfigurableSession session -> Ok session.ConfigName
            | PersistentSession _ -> Error <| CustomError "The session is already in progress"

        let playerName = ""
        let target = Model.tryCreateJoiningPlayer id playerName
        let player = { GameSessionId = id; GameSessionName = sessionName; Name = playerName; Target = target }

        return Model.simple page (JoiningSession player)
    }

    let asMessage page =
        function
        | Ok result ->
            Message.setModel result
        | Error (PersistenceError (GetSessionError (SessionNotFound _))) ->
            Message.setModel <| Model.simple page JoiningInvalidSession
        | Error e ->
            Message.setError e

    result |> Async.map (asMessage page)
}

let createEventLoop id : Flow<unit> = monad {
    let! mediator = Env.askMediator
    do! cmd <| Cmd.ofSub (fun dispatch -> mediator |> EventMediator.subscribe id (Message.receiveEvent >> dispatch))
}

let setJoinPage id page model : Flow<Model> = monad {
    do! asyncCmd <| getSessionToJoin id
    do! createEventLoop (GameSessionId id)

    return { model with Page = page }
}

let private loadJoinPageAwaitingState player state model : Flow<Model> = monad {
    let id = player.ValidGameSessionId
    do! createEventLoop id
    return { model with State = state; LocalState = None; IsLoaded = true }
}

let private loadConfiguringState id state model : Flow<Model> = monad {
    do! createEventLoop id
    return { model with State = state; LocalState = None; IsLoaded = true }
}

let private loadAwaitingGameStartState (state : GameSessionState) model : Flow<Model> = monad {
    do! createEventLoop (state.GameSession |> GameSession.id)
    return { model with State = AwaitingGameStart state; LocalState = None; IsLoaded = true }
}

let private loadAwaitingTurnState state model : Flow<Model> = monad {
    do! createEventLoop (state.GameSession |> GameSession.id)
    return { model with State = AwaitingTurn state; LocalState = None; IsLoaded = true }
}

let private loadBettingState state model : Flow<Model> = monad {
    do! createEventLoop (state.GameState.GameSession |> GameSession.id)
    return { model with State = Betting state; LocalState = None; IsLoaded = true }
}

let loadState state model =
    match model.Page, state with
    | StartPage, state ->
        let newState = Model.createAddSessionState "" ""
        { model with State = AddingSessionName newState; LocalState = Some state; IsLoaded = true } |> pureFlow
    | JoinPage _, NoState ->
        { model with LocalState = None; IsLoaded = true } |> pureFlow
    | JoinPage _, (AwaitingJoinConfirmation player | AwaitingGameSessionStart player as state) ->
        model |> loadJoinPageAwaitingState player state
    | ConfigurePage, (ConfiguringSession { Config = { ConfigId = id } } as state) ->
        model |> loadConfiguringState id state
    | PlayPage, AwaitingGameStart state ->
        model |> loadAwaitingGameStartState state
    | PlayPage, AwaitingTurn state ->
        model |> loadAwaitingTurnState state
    | PlayPage, Betting state ->
        model |> loadBettingState state
    | _, NoState ->
        { model with Page = HomePage; LocalState = None; IsLoaded = true } |> pureFlow
    | _ ->
        { model with Page = HomePage; LocalState = Some state; IsLoaded = true } |> pureFlow

let private recoverConfiguringSession id state model : Flow<Model> = monad {
    do! createEventLoop id
    return { model with Page = ConfigurePage; State = state; LocalState = None }
}

let private recoverJoinAwaitingConfirmation player state model : Flow<Model> = monad {
    do! createEventLoop player.ValidGameSessionId
    let (GameSessionId rawId) = player.ValidGameSessionId
    return { model with Page = JoinPage rawId; State = state; LocalState = None }
}

let private recoverAwaitingGameStart (state : GameSessionState) model : Flow<Model> = monad {
    do! createEventLoop (state.GameSession |> GameSession.id)
    return { model with Page = PlayPage; State = AwaitingGameStart state; LocalState = None }
}

let private recoverAwaitingTurn state model : Flow<Model> = monad {
    do! createEventLoop (state.GameSession |> GameSession.id)
    return { model with Page = PlayPage; State = AwaitingTurn state; LocalState = None }
}

let private recoverBetting state model : Flow<Model> = monad {
    do! createEventLoop (state.GameState.GameSession |> GameSession.id)
    return { model with Page = PlayPage; State = Betting state; LocalState = None }
}

let recoverLocalState model =
    match model.LocalState with
    | Some (ConfiguringSession { Config = { ConfigId = id } } as state) ->
        model |> recoverConfiguringSession id state
    | Some (AwaitingJoinConfirmation player | AwaitingGameSessionStart player as state) ->
        model |> recoverJoinAwaitingConfirmation player state
    | Some (AwaitingGameStart state) ->
        model |> recoverAwaitingGameStart state
    | Some (AwaitingTurn state) ->
        model |> recoverAwaitingTurn state
    | Some (Betting state) ->
        model |> recoverBetting state
    | _ ->
        { model with LocalState = None } |> pureFlow

let ignoreLocalState model =
    { model with LocalState = None } |> pureFlow

let clearLocalState model : Flow<Model> = monad {
    do! clearStateSimple
    return! model |> ignoreLocalState
}

let updateSession state model : Flow<Model> = monad {
    let! storage = Env.askStorage
    let! repo = Env.askRepo

    let result = asyncResult {
        do! repo |> Persistence.updateSession (ConfigurableSession state.Config)
        let localState = ConfiguringSession { state with Target = GameSession.fromConfig state.Config }
        do! storage.SetState localState
        return Message.setModel { model with Page = ConfigurePage; State = localState }
    }

    do! cmd (result |> Message.handleAsyncError |> Cmd.OfAsync.result)

    return model
}
