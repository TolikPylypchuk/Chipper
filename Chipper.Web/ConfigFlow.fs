module Chipper.Web.ConfigFlow

open FSharpPlus

open FsToolkit.ErrorHandling

open Elmish

open Chipper.Core.Domain
open Chipper.Web

let private isUnique (players : Player list) playerName =
    players |> List.exists (fun player -> player.Name = playerName) |> not

let private apendNumberIfNotUnique players playerName =
    let isUnique = isUnique players
    if playerName |> isUnique then
        playerName
    else
        2
        |> Seq.unfold (fun num -> Some (playerName |> PlayerName.appendNumber num, num + 1))
        |> Seq.filter isUnique
        |> Seq.head

let acceptPlayerRequest playerId state model = monad {
    let name =
        state.Config.ConfigPlayerRequests
        |> List.filter (fun request -> request.PlayerId = playerId)
        |> List.map (fun request -> request.Info.PlayerName)
        |> List.tryHead

    match name with
    | Some name ->
        let actualName = name |> apendNumberIfNotUnique (state.Config.ConfigHost :: state.Config.ConfigPlayers)
        let newPlayer = { Id = playerId; Name = actualName; Chips = [] }
        let playerRequests =
            state.Config.ConfigPlayerRequests
            |> List.filter (fun request -> request.PlayerId <> playerId)

        let newState =
            {
                state with
                    Config = {
                        state.Config with
                            ConfigPlayers = state.Config.ConfigPlayers @ [ newPlayer ]
                            ConfigPlayerRequests = playerRequests
                    }
            }

        do! PlayerAccepted playerId |> postEvent newState.Config.ConfigId

        let! result = model |> Flow.updateSession newState

        if actualName <> name then
            let renameInfo = { PlayerId = playerId; NewName = actualName; HostName = PlayerName.theGame }
            do! PlayerRenamed renameInfo |> postEvent newState.Config.ConfigId
            return result
        else
            return result
    | None ->
        return model
}

let rejectPlayerRequest playerId state model = monad {
    let playerRequests = state.Config.ConfigPlayerRequests |> List.filter (fun request -> request.PlayerId <> playerId)
    let newState = { state with Config = { state.Config with ConfigPlayerRequests = playerRequests } }

    do! PlayerRejected playerId |> postEvent newState.Config.ConfigId

    return { model with State = ConfiguringSession newState }
}

let editSessionName state model =
    let (GameSessionName name) = state.Config.ConfigName
    { model with State = ConfiguringSession { state with EditMode = EditSession name } } |> pureFlow

let inputSessionName sessionName state model =
    { model with State = ConfiguringSession { state with EditMode = EditSession sessionName } } |> pureFlow

let private doAcceptSessionNameEdit newName state : Flow<ConfigSessionState> = monad {
    let newState = { state with Config = { state.Config with ConfigName = newName }; EditMode = NoEdit }
    do! GameSessionNameChanged newName |> postEvent newState.Config.ConfigId

    return newState
}

let acceptSessionNameEdit sessionName state model = monad {
    let! newState =
        match sessionName |> GameSessionName.create with
        | Ok newName -> doAcceptSessionNameEdit newName state
        | _ -> state |> pureFlow

    return! model |> Flow.updateSession newState
}

let editPlayerName playerId state model =
    let name =
        (state.Config.ConfigHost :: state.Config.ConfigPlayers)
        |> List.filter (fun player -> player.Id = playerId)
        |> List.map (fun player -> player.Name)
        |> List.tryHead

    match name with
    | Some name ->
        let newState = { state with EditMode = EditPlayer (playerId, name |> PlayerName.value) }
        { model with State = ConfiguringSession newState }
    | None ->
        model
    |> pureFlow

let inputPlayerName playerName editedName state model =
    { model with State = ConfiguringSession { state with EditMode = EditPlayer (playerName, editedName) } } |> pureFlow

let private doAcceptPlayerNameEdit playerId newName state : Flow<ConfigSessionState> = monad {
    let newPlayers =
        state.Config.ConfigPlayers
        |> List.map (fun player -> if player.Id = playerId then { player with Name = newName } else player)

    let host =
        if state.Config.ConfigHost.Id = playerId
        then { state.Config.ConfigHost with Name = newName }
        else state.Config.ConfigHost

    let newState =
        {
            state with
                Config = { state.Config with ConfigPlayers = newPlayers; ConfigHost = host }
                EditMode = NoEdit
        }

    let renameInfo =  { HostName = newState.Config.ConfigHost.Name; PlayerId = playerId; NewName = newName }
    do! PlayerRenamed renameInfo |> postEvent newState.Config.ConfigId

    return newState
}

let acceptPlayerNameEdit playerName editedName state model = monad {
    let! newState =
        match editedName |> PlayerName.create with
        | Ok newName -> doAcceptPlayerNameEdit playerName newName state
        | _ -> state |> pureFlow

    return! model |> Flow.updateSession newState
}

let cancelEdit state model =
    { model with State = ConfiguringSession { state with EditMode = NoEdit } } |> pureFlow

let isEditedPlayerNameValid players playerId editedName =
    match editedName |> PlayerName.create with
    | Ok name -> players |> List.forall (fun (player : Player) -> player.Id = playerId || player.Name <> name)
    | _ -> false

let isEditedSessionNameValid = GameSessionName.create >> function Ok _ -> true | _ -> false

let removePlayer playerId state model = monad {
    let players = state.Config.ConfigPlayers |> List.filter (fun player -> player.Id <> playerId)
    let newState = { state with Config = { state.Config with ConfigPlayers = players } }
    
    do! PlayerRemoved playerId |> postEvent newState.Config.ConfigId

    return! model |> Flow.updateSession newState
}
