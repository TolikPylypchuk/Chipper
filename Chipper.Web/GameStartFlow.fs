module Chipper.Web.GameStartFlow

open FSharpPlus

open FsToolkit.ErrorHandling

open Elmish

open Chipper.Core
open Chipper.Web

let startSession model =
    { model with Page = StartPage; State = AddingSessionName ("", "") }, Cmd.none

let startSessionWhenConfiguring model =
    { model with Page = StartPage }, Cmd.none

let inputSessionName name playerName model =
    { model with State = AddingSessionName (name, playerName) }, Cmd.none

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
