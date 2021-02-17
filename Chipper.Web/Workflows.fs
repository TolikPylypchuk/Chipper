module Chipper.Web.Workflows

open FsToolkit.ErrorHandling

open Chipper.Core
open Chipper.Core.Domain
open Chipper.Core.Persistence

let getState storage repo = async {
    let! localState = storage.GetState ()
    let! currentState = async {
        match localState with
        | StartingSession newSession ->
            match! repo |> getSession newSession.Id with
            | Ok (NewSession newSession) -> return StartingSession newSession |> Some
            | _ -> return None
        | _ -> return localState |> Some
    }

    match currentState with
    | Some state ->
        if state <> localState then
            do! storage.SetState state
        return state
    | None ->
        do! storage.ClearState ()
        return NoState
}

let startNewSession storage repo model name =
    let result = asyncResult {
        let! name = gameSessionName name
        let! newSession = repo |> createSession name

        let state = StartingSession newSession
        do! storage.SetState state
        return SetModel { model with Page = InvitePage; State = state }
    }

    result |> handleAsyncMessageError

let getSessionToJoin id repo =
    let page = JoinPage id
    let id = GameSessionId id

    let result = asyncResult {
        let! session = repo |> getSession id

        let! name = result {
            match session with
            | NewSession session -> return session.Name
            | ConfigurableSession session -> return session.Name
            | PersistentSession _ -> return! Error <| CustomError "The session is already in progress"
        }

        let state = JoiningSession { GameSessionId = id; GameSessionName = name; Name = "" }
        return { Page = page; State = state }
    }

    let asMessage =
        function
        | Ok result ->
            SetModel result
        | Error (PersistenceError (GetSessionError (SessionNotFound _))) ->
            SetModel { Page = page; State = JoiningInvalidSession }
        | Error e ->
            SetError e

    result |> Async.map asMessage
