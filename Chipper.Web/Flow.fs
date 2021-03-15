module Chipper.Web.Flow

open FSharpPlus
open FSharpPlus.Data

open FsToolkit.ErrorHandling
open Elmish

open Chipper.Core
open Chipper.Core.Domain
open Chipper.Core.Persistence

let doNothing model = model, Cmd.none

let setPage page model =
    { model with Page = page }, Cmd.none

let setError e model =
    printfn "An unhandled error appeared: %O" e
    model |> doNothing

let asMessage page =
    function
    | Ok result ->
        Message.setModel result
    | Error (PersistenceError (GetSessionError (SessionNotFound _))) ->
        Message.setModel <| Model.simple page JoiningInvalidSession
    | Error e ->
        Message.setError e

let getState = monad {
    let! storage = Env.askStorage
    let! repo = Env.askRepo

    return async {
        let! localState = storage.GetState ()
        let! currentState = async {
            match localState with
            | ConfiguringSession { Config = config } ->
                match! repo |> getSession config.ConfigId with
                | Ok (ConfigurableSession config) ->
                    let state = { Config = config; PlayerRequests = []; EditMode = NoEdit }
                    return ConfiguringSession state |> Some
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
}

let getSessionToJoin id = monad {
    let! repo = Env.askRepo
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

    result |> Async.map (asMessage page)
}

let setJoinPage id page model = monad {
    let! session = getSessionToJoin id
    return { model with Page = page }, Cmd.OfAsync.result session
}

let loadState state model =
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

let createEventLoop id = monad {
    let! mediator = Env.askMediator
    return Cmd.ofSub (fun dispatch -> mediator |> EventMediator.subscribe id (Message.receiveEvent >> dispatch))
}

let recoverLocalState model =
    match model.LocalState with
    | Some (ConfiguringSession { Config = { ConfigId = id } } as state) ->
        createEventLoop id |>> fun loop -> { model with Page = ConfigurePage; State = state; LocalState = None }, loop
    | _ ->
        ({ model with LocalState = None }, Cmd.none) |> Env.none

let ignoreLocalState model =
    { model with LocalState = None }, Cmd.none

let clearLocalState model = monad {
    let! storage = Env.askStorage
    Async.StartImmediate <| storage.ClearState ()
    return model |> ignoreLocalState
}

let receiveEvent event model =
    match event, model.State with
    | PlayerAccessRequested joinInfo, ConfiguringSession state ->
        model |> EventFlow.onPlayerAccessRequested state joinInfo

    | PlayerAccepted playerName, AwaitingJoinConfirmation player when player.ValidName = playerName ->
        model |> EventFlow.onPlayerAccepted player

    | PlayerRejected playerName, AwaitingJoinConfirmation player when player.ValidName = playerName ->
        model |> EventFlow.onPlayerRejected player

    | PlayerRenamed renameInfo, AwaitingGameStart player when player.ValidName = renameInfo.OldName ->
        model |> EventFlow.onPlayerRenamed player renameInfo

    | PlayerRemoved playerName, AwaitingGameStart player when player.ValidName = playerName ->
        model |> EventFlow.onPlayerRemoved player

    | _ ->
        model |> doNothing

let setStateSimple state = Env.askStorage |>> fun storage -> storage.SetState state |> Async.StartImmediate
