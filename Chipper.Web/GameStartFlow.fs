module Chipper.Web.GameStartFlow

open FSharpPlus

open FsToolkit.ErrorHandling

open Elmish

open Chipper.Core
open Chipper.Core.Domain
open Chipper.Web

let startSession model =
    { model with Page = StartPage; State = AddingSessionName ("", "") }, Cmd.none

let startSessionWhenConfiguring model =
    { model with Page = StartPage }, Cmd.none

let inputSessionName name playerName model =
    { model with State = AddingSessionName (name, playerName) }, Cmd.none

let inputPlayerNameWhenAddingSessionName name playerName model =
    { model with State = AddingSessionName (name, playerName) }, Cmd.none

let inputPlayerNameWhenJoiningSession id sessionName name model =
    let state = JoiningSession { GameSessionId = id; GameSessionName = sessionName; Name = name }
    { model with State = state }, Cmd.none

let saveNewSession name playerName' = monad {
    let! repo = Env.askRepo

    let result = asyncResult {
        let! name = gameSessionName name
        let! playerName' = playerName playerName'
        let! config = repo |> createSession name playerName'

        return Message.sessionSaved config
    }

    return result |> Message.handleAsyncError
}

let saveSessionName name playerName model = monad {
    let! newSession = saveNewSession name playerName
    return model, Cmd.OfAsync.result newSession
}

let saveSessionNameWhenConfiguring model =
    { model with Page = ConfigurePage }, Cmd.none

let onSessionSaved config model = monad {
    let state = ConfiguringSession { Config = config; PlayerRequests = []; EditMode = NoEdit }

    do! Flow.setStateSimple state
    let! loop = Flow.createEventLoop config.ConfigId

    return { model with Page = ConfigurePage; State = state }, loop
}

let private doRequestAccess player joinInfo = monad {
    let! mediator = Env.askMediator
    mediator |> EventMediator.post (PlayerAccessRequested joinInfo) player.ValidGameSessionId
    return AwaitingJoinConfirmation player
}

let requestAccess player joinInfo model = monad {
    let! storage = Env.askStorage
    let validPlayer = player |> ValidJoiningPlayer.create joinInfo
    let! newState = doRequestAccess validPlayer joinInfo

    let! loop = Flow.createEventLoop player.GameSessionId
    Async.StartImmediate <| storage.SetState newState

    return { model with State = newState }, loop
}

let requestAccessAgain player joinInfo model = monad {
    let! newState = doRequestAccess player joinInfo
    return { model with State = newState }, Cmd.none
}

let acceptRename player model =
    { model with State = AwaitingGameStart player }, Cmd.none
