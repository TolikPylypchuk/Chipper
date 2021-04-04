module Chipper.Web.GameStartFlow

open FSharpPlus
open FSharpPlus.Data

open FsToolkit.ErrorHandling

open Elmish

open Chipper.Core
open Chipper.Core.Domain
open Chipper.Web

let startSession model =
    { model with Page = StartPage; State = AddingSessionName ("", "") } |> pureFlow

let startSessionWhenConfiguring model =
    { model with Page = StartPage } |> pureFlow

let inputSessionName name playerName model =
    { model with State = AddingSessionName (name, playerName) } |> pureFlow

let inputPlayerNameWhenAddingSessionName name playerName model =
    { model with State = AddingSessionName (name, playerName) } |> pureFlow

let inputPlayerNameWhenJoiningSession id sessionName name model =
    let state = JoiningSession { GameSessionId = id; GameSessionName = sessionName; Name = name }
    { model with State = state } |> pureFlow

let saveNewSession name playerName' : Flow<Async<Message>> = monad {
    let! repo = Env.askRepo

    let result = asyncResult {
        let! name = gameSessionName name
        let! playerName' = playerName playerName'
        let! config = repo |> createSession name playerName'

        return Message.sessionSaved config
    }

    return result |> Message.handleAsyncError
}

let saveSessionName name playerName model : Flow<Model> = monad {
    do! asyncCmd <| saveNewSession name playerName
    return model
}

let saveSessionNameWhenConfiguring model =
    { model with Page = ConfigurePage } |> pureFlow

let onSessionSaved config model : Flow<Model> = monad {
    let state = ConfiguringSession { Config = config; EditMode = NoEdit }

    do! Flow.setStateSimple state
    do! Flow.createEventLoop config.ConfigId

    return { model with Page = ConfigurePage; State = state }
}

let private doRequestAccess player request : Flow<LocalState> = monad {
    do! PlayerAccessRequested request |> postEvent player.ValidGameSessionId
    return AwaitingJoinConfirmation player
}

let requestAccess player joinInfo model = monad {
    let! repo = Env.askRepo

    let request : Async<Flow<Model>> = async {
        let! id = repo.GeneratePlayerId ()
        let request = { PlayerId = id; Info = joinInfo }
        let validPlayer = player |> ValidJoiningPlayer.create request
        return monad {
            let! newState = doRequestAccess validPlayer request
            do! Flow.setStateSimple newState
            return { model with State = newState }
        }
    }

    do! continueWithAsync request
    return model
}

let requestAccessAgain player joinInfo model : Flow<Model> = monad {
    let! newState = doRequestAccess player joinInfo
    return { model with State = newState }
}

let acceptRename player model =
    { model with State = AwaitingGameStart player } |> pureFlow

let cancelRequest player model : Flow<Model> = monad {
    do! Flow.clearStateSimple
    do! PlayerRequestCanceled player.ValidId |> postEvent player.ValidGameSessionId
    return { model with State = JoinRequestCanceled player.ValidGameSessionName }
}
