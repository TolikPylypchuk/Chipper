module Chipper.Web.Flow

open FSharpPlus

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

let private getConfigSessionState repo config = async {
    match! repo |> getSession config.ConfigId with
    | Ok (ConfigurableSession config) ->
        let state = { Config = config; EditMode = NoEdit }
        return ConfiguringSession state |> Some
    | _ ->
        return None
}

let private getJoinConfirmationState repo player = async {
    match! repo |> getSession player.ValidGameSessionId with
    | Ok (ConfigurableSession config) ->
        let player =
            { player with
                ValidGameSessionId = config.ConfigId
                ValidGameSessionName = config.ConfigName
            }

        let localState =
            if config.ConfigPlayers |> List.exists (fun player' -> player'.Name = player.ValidName)
            then AwaitingGameStart player
            else AwaitingJoinConfirmation player

        return Some localState
    | _ ->
        return None
}

let private getGameStartState repo player = async {
    match! repo |> getSession player.ValidGameSessionId with
    | Ok (ConfigurableSession config) ->
        let player =
            { player with
                ValidGameSessionId = config.ConfigId
                ValidGameSessionName = config.ConfigName
            }

        return Some <| AwaitingGameStart player
    | _ ->
        return None
}

let getState = monad {
    let! storage = Env.askStorage
    let! repo = Env.askRepo

    return async {
        let! localState = storage.GetState ()
        let! currentState =
            match localState with
            | ConfiguringSession { Config = config } -> config |> getConfigSessionState repo
            | AwaitingJoinConfirmation player -> player |> getJoinConfirmationState repo
            | AwaitingGameStart player -> player |> getGameStartState repo
            | _ -> async.Return None

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

let setStateSimple state =
    Env.askStorage |>> fun storage -> storage.SetState state |> Async.StartImmediate

let clearStateSimple =
    Env.askStorage |>> fun storage -> storage.ClearState () |> Async.StartImmediate

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

let createEventLoop id = monad {
    let! mediator = Env.askMediator
    return Cmd.ofSub (fun dispatch -> mediator |> EventMediator.subscribe id (Message.receiveEvent >> dispatch))
}

let loadState state model =
    match model.Page, state with
    | StartPage, state ->
        let newState = AddingSessionName ("", "")
        ({ model with State = newState; LocalState = Some state; IsLoaded = true }, Cmd.none) |> Env.none
    | JoinPage _, NoState ->
        ({ model with LocalState = None; IsLoaded = true }, Cmd.none) |> Env.none
    | JoinPage _, (AwaitingJoinConfirmation player | AwaitingGameStart player as state) ->
        let id = player.ValidGameSessionId
        createEventLoop id |>> fun loop -> { model with State = state; LocalState = None; IsLoaded = true }, loop
    | ConfigurePage, (ConfiguringSession { Config = { ConfigId = id } } as state) ->
        createEventLoop id |>> fun loop -> { model with State = state; LocalState = None; IsLoaded = true }, loop
    | _, NoState ->
        ({ model with Page = HomePage; LocalState = None; IsLoaded = true }, Cmd.none) |> Env.none
    | _ ->
        ({ model with Page = HomePage; LocalState = Some state; IsLoaded = true }, Cmd.none) |> Env.none

let recoverLocalState model =
    match model.LocalState with
    | Some (ConfiguringSession { Config = { ConfigId = id } } as state) ->
        createEventLoop id |>> fun loop -> { model with Page = ConfigurePage; State = state; LocalState = None }, loop
    | Some (AwaitingJoinConfirmation player | AwaitingGameStart player as state) ->
        createEventLoop player.ValidGameSessionId |>> fun loop ->
            let (GameSessionId rawId) = player.ValidGameSessionId
            { model with Page = JoinPage rawId; State = state; LocalState = None }, loop
    | _ ->
        ({ model with LocalState = None }, Cmd.none) |> Env.none

let ignoreLocalState model =
    { model with LocalState = None }, Cmd.none

let clearLocalState model = monad {
    let! storage = Env.askStorage
    Async.StartImmediate <| storage.ClearState ()
    return model |> ignoreLocalState
}
