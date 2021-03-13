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
    Cmd.ofSub (fun dispatch -> mediator |> EventMediator.subscribe id (ReceiveEvent >> dispatch))

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
        let state = ConfiguringSession { Config = config; PlayerRequests = []; EditMode = NoEdit }
        do! storage.SetState state
        return SetModel { model with Page = ConfigurePage; State = state }
    }
    
    result |> handleAsyncMessageError

let acceptPlayerRequest mediator state playerName =
    let newPlayer = { Name = playerName; Chips = [] }
    let playerRequests = state.PlayerRequests |> List.filter (fun joinInfo -> joinInfo.PlayerName <> playerName)

    let newState =
        { state with
            Config = { state.Config with ConfigPlayers = state.Config.ConfigPlayers @ [ newPlayer ] }
            PlayerRequests = playerRequests
        }

    mediator |> EventMediator.post (PlayerAccepted playerName) newState.Config.ConfigId

    ConfiguringSession newState

let rejectPlayerRequest mediator state playerName =
    let playerRequests = state.PlayerRequests |> List.filter (fun joinInfo -> joinInfo.PlayerName <> playerName)
    let newState = { state with PlayerRequests = playerRequests }

    mediator |> EventMediator.post (PlayerRejected playerName) newState.Config.ConfigId

    ConfiguringSession newState

let isEditedPlayerNameValid players originalName editedName =
    match editedName |> PlayerName.create with
    | Ok name -> players |> List.forall (fun (player : Player) -> player.Name = originalName || player.Name <> name)
    | _ -> false

let editPlayerName mediator state playerName editedName =
    match editedName |> PlayerName.create with
    | Ok newName ->
        let newPlayers =
            state.Config.ConfigPlayers
            |> List.map (fun player -> if player.Name = playerName then { player with Name = newName } else player)

        let host =
            if state.Config.ConfigHost.Name = playerName
            then { state.Config.ConfigHost with Name = newName }
            else state.Config.ConfigHost

        let newState =
            {
                state with
                    Config = { state.Config with ConfigPlayers = newPlayers; ConfigHost = host }
                    EditMode = NoEdit
            }

        mediator |> EventMediator.post (PlayerRenamed (playerName, newName)) newState.Config.ConfigId

        ConfiguringSession newState
    | _ ->
        ConfiguringSession state

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

let createValidPlayer joinInfo (player : JoiningPlayer) =
    {
        ValidGameSessionId = player.GameSessionId
        ValidGameSessionName = player.GameSessionName
        ValidName = joinInfo.PlayerName
    }

let requestAccess mediator joinInfo (player : ValidJoiningPlayer) =
    mediator |> EventMediator.post (PlayerAccessRequested joinInfo) player.ValidGameSessionId
    AwaitingJoinConfirmation player
