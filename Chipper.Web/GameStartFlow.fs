module Chipper.Web.GameStartFlow

open FSharpPlus

open Elmish

open Chipper.Core
open Chipper.Web

let startSession model =
    { model with Page = StartPage; State = AddingSessionName <| Model.createAddSessionState "" "" } |> pureFlow

let startSessionWhenConfiguring model =
    { model with Page = StartPage } |> pureFlow

let inputSessionName sessionName hostName model =
    { model with State = AddingSessionName <| Model.createAddSessionState sessionName hostName } |> pureFlow

let inputPlayerNameWhenAddingSessionName sessionName hostName model =
    { model with State = AddingSessionName <| Model.createAddSessionState sessionName hostName } |> pureFlow

let inputPlayerNameWhenJoiningSession id sessionName name model =
    let target = Model.tryCreateJoiningPlayer id name
    let player = { GameSessionId = id; GameSessionName = sessionName; Name = name; Target = target }
    { model with State = JoiningSession player } |> pureFlow

let saveSessionName sessionName hostName model : Flow<Model> = monad {
    let! repo = Env.askRepo
    let createSession = Persistence.createSession sessionName hostName
    let mapToMessage = Result.map Message.sessionSaved >> Message.handleError

    do! cmd <| Cmd.OfAsync.perform createSession repo mapToMessage

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
        let validPlayer = player |> Model.createValidJoiningPlayer request
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
