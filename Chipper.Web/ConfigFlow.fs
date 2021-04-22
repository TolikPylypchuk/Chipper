module Chipper.Web.ConfigFlow

open FSharpPlus
open FSharpPlus.Data

open FsToolkit.ErrorHandling

open Chipper.Core
open Chipper.Web

let private doAcceptPlayerRequest playerId state name model = monad {
    let playerRequests =
        state.Config.ConfigPlayerRequests
        |> List.filter (fun request -> request.PlayerId <> playerId)

    let newPlayers, actualName = state.Config.ConfigPlayers |> PlayerList.addPlayer playerId name

    let newState =
        { state with Config = { state.Config with ConfigPlayers = newPlayers; ConfigPlayerRequests = playerRequests } }

    do! PlayerAccepted playerId |> postEvent newState.Config.ConfigId

    let! result = model |> Flow.updateSession newState

    if actualName <> name then
        let renameInfo = { PlayerId = playerId; NewName = actualName; HostName = PlayerName.theGame }
        do! PlayerRenamed renameInfo |> postEvent newState.Config.ConfigId
        return result
    else
        return result
}

let acceptPlayerRequest playerId state model = monad {
    let name =
        state.Config.ConfigPlayerRequests
        |> List.filter (fun request -> request.PlayerId = playerId)
        |> List.map (fun request -> request.Info.PlayerName)
        |> List.tryHead

    match name with
    | Some name ->
        return! model |> doAcceptPlayerRequest playerId state name
    | None ->
        return model
}

let rejectPlayerRequest playerId state model = monad {
    let playerRequests = state.Config.ConfigPlayerRequests |> List.filter (fun request -> request.PlayerId <> playerId)
    let newState = { state with Config = { state.Config with ConfigPlayerRequests = playerRequests } }

    do! PlayerRejected playerId |> postEvent newState.Config.ConfigId

    return { model with State = ConfiguringSession newState }
}

let private createEditedSession name =
    { Name = name; Target = name |> Domain.gameSessionName }

let editSessionName state model =
    let (GameSessionName name) = state.Config.ConfigName
    { model with State = ConfiguringSession { state with EditMode = EditSession <| createEditedSession name } }
    |> pureFlow

let inputSessionName name state model =
    { model with State = ConfiguringSession { state with EditMode = EditSession <| createEditedSession name } }
    |> pureFlow

let private doAcceptSessionNameEdit newName state : Flow<ConfigSessionState> = monad {
    let newState = { state with Config = { state.Config with ConfigName = newName }; EditMode = NoEdit }
    do! GameSessionNameChanged newName |> postEvent newState.Config.ConfigId

    return newState
}

let acceptSessionNameEdit newName state model = monad {
    let! newState = doAcceptSessionNameEdit newName state
    return! model |> Flow.updateSession newState
}

let editPlayerName playerId state model =
    let name =
        state.Config.ConfigPlayers
        |> PlayerList.configValue
        |> NonEmptyList.toList
        |> List.filter (fun player -> player.Id = playerId)
        |> List.map (fun player -> player.Name)
        |> List.tryHead

    match name with
    | Some name ->
        let editedPlayer = { Id = playerId; Name = name |> PlayerName.value; Target = Ok state.Config.ConfigPlayers }
        let newState = { state with EditMode = EditPlayer editedPlayer }
        { model with State = ConfiguringSession newState }
    | None ->
        model
    |> pureFlow

let inputPlayerName playerId editedName state model =
    let target = editedName |> Domain.playerName >>= Domain.editPlayerName playerId state.Config.ConfigPlayers

    let editedPlayer = { Id = playerId; Name = editedName; Target = target }
    { model with State = ConfiguringSession { state with EditMode = EditPlayer editedPlayer } } |> pureFlow

let private doAcceptPlayerNameEdit playerId newPlayers state : Flow<ConfigSessionState> = monad {
    let newState = { state with Config = { state.Config with ConfigPlayers = newPlayers }; EditMode = NoEdit }

    let renamedPlayer =
        newPlayers
        |> PlayerList.configPlayers
        |> List.filter (fun player -> player.Id = playerId)
        |> List.tryHead

    match renamedPlayer with
    | Some player ->
        let renameInfo =
            {
                HostName = (newState.Config.ConfigPlayers |> PlayerList.configHost).Name
                PlayerId = playerId
                NewName = player.Name
            }

        do! PlayerRenamed renameInfo |> postEvent newState.Config.ConfigId

        return newState
    | None ->
        return newState
}

let acceptPlayerNameEdit playerId newPlayers state model = monad {
    let! newState = doAcceptPlayerNameEdit playerId newPlayers state
    return! model |> Flow.updateSession newState
}

let cancelEdit state model =
    { model with State = ConfiguringSession { state with EditMode = NoEdit } } |> pureFlow

let removePlayer playerId state model = monad {
    let players = state.Config.ConfigPlayers |> PlayerList.removePlayer playerId
    let newState = { state with Config = { state.Config with ConfigPlayers = players } }
    
    do! PlayerRemoved playerId |> postEvent newState.Config.ConfigId

    return! model |> Flow.updateSession newState
}

let movePlayer move playerId state model =
    let players = state.Config.ConfigPlayers |> move playerId
    let newState = { state with Config = { state.Config with ConfigPlayers = players } }

    { model with State = ConfiguringSession newState } |> pureFlow
