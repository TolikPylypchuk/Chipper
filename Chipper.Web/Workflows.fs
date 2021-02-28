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
        | ConfiguringSession (config, playerRequests) ->
            match! repo |> getSession config.ConfigId with
            | Ok (ConfigurableSession config) -> return ConfiguringSession (config, playerRequests) |> Some
            | _ -> return None
        | _ -> return None
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

let setStateSimple state storage = storage.SetState state |> Async.StartImmediate

let loadState model state =
    match model.Page, state with
    | StartPage, state ->
        { model with State = AddingSessionName ("", ""); LocalState = Some state; IsLoaded = true }, Cmd.none
    | JoinPage _, NoState ->
        { model with LocalState = None; IsLoaded = true }, Cmd.none
    | JoinPage _, state ->
        { model with LocalState = Some state; IsLoaded = true }, Cmd.none
    | ConfigurePage, (ConfiguringSession _ as state) ->
        { model with State = state; LocalState = None; IsLoaded = true }, Cmd.none
    | _, NoState ->
        { model with Page = HomePage; LocalState = None; IsLoaded = true }, Cmd.none
    | _ ->
        { model with Page = HomePage; LocalState = Some state; IsLoaded = true }, Cmd.none

let createEventLoop id mediator =
    Cmd.ofSub (fun dispatch -> mediator |> EventMediator.subscribe id (ReceiveEvent >> dispatch) |> ignore)

let saveNewSession repo name playerName' =
    let result = asyncResult {
        let! name = gameSessionName name
        let! playerName' = playerName playerName'
        let! config = repo |> createSession name playerName'

        return SessionSaved config
    }

    result |> handleAsyncMessageError

let configureSession storage repo model config =
    let result = asyncResult {
        do! repo |> updateSession (ConfigurableSession config)
        let state = ConfiguringSession (config, [])
        do! storage.SetState state
        return SetModel { model with Page = ConfigurePage; State = state }
    }
    
    result |> handleAsyncMessageError

let getSessionToJoin id repo =
    let page = JoinPage id
    let id = GameSessionId id

    let result = asyncResult {
        let! session = repo |> getSession id

        let! name = result {
            match session with
            | ConfigurableSession session -> return session.ConfigName
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
