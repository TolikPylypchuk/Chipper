module Chipper.Web.Workflows

open FsToolkit.ErrorHandling

open Elmish

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

let loadState model state =
    match model.Page, state with
    | InvitePage, (StartingSession _ as state) ->
        { model with State = state; LocalState = None; IsLoaded = true }, Cmd.none
    | JoinPage _, NoState ->
        { model with LocalState = None; IsLoaded = true }, Cmd.none
    | JoinPage _, state ->
        { model with LocalState = Some state; IsLoaded = true }, Cmd.none
    | _, NoState ->
        { model with Page = HomePage; LocalState = None; IsLoaded = true }, Cmd.none
    | _ ->
        { model with Page = HomePage; LocalState = Some state; IsLoaded = true }, Cmd.none

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
        return Model.simple page state
    }

    let asMessage =
        function
        | Ok result ->
            SetModel result
        | Error (PersistenceError (GetSessionError (SessionNotFound _))) ->
            SetModel <| Model.simple page JoiningInvalidSession
        | Error e ->
            SetError e

    result |> Async.map asMessage
