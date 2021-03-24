module Chipper.Web.ConfigFlow

open FSharpPlus

open FsToolkit.ErrorHandling

open Elmish

open Chipper.Core.Domain
open Chipper.Web

let setBettingType bettingType state =
    Flow.updateSession { state with Config = { state.Config with ConfigBettingType = bettingType } }

let setRaiseType raiseType state =
    Flow.updateSession { state with Config = { state.Config with ConfigRaiseType = raiseType } }

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

        do! Env.askMediator |>> EventMediator.post (PlayerAccepted playerId) newState.Config.ConfigId

        if actualName <> name then
            let renameInfo = { PlayerId = playerId; NewName = actualName; HostName = PlayerName.theGame }
            do! Env.askMediator |>> EventMediator.post (PlayerRenamed renameInfo) newState.Config.ConfigId

        return! model |> Flow.updateSession newState
    | None ->
        return model, Cmd.none
}

let rejectPlayerRequest playerId state model = monad {
    let playerRequests = state.Config.ConfigPlayerRequests |> List.filter (fun request -> request.PlayerId <> playerId)
    let newState = { state with Config = { state.Config with ConfigPlayerRequests = playerRequests } }
    
    do! Env.askMediator |>> EventMediator.post (PlayerRejected playerId) newState.Config.ConfigId

    return { model with State = ConfiguringSession newState }, Cmd.none
}

let editSessionName state model =
    let (GameSessionName name) = state.Config.ConfigName
    { model with State = ConfiguringSession { state with EditMode = EditSession name } }, Cmd.none

let inputSessionName sessionName state model =
    { model with State = ConfiguringSession { state with EditMode = EditSession sessionName } }, Cmd.none

let private doAcceptSessionNameEdit newName state = monad {
    let newState = { state with Config = { state.Config with ConfigName = newName }; EditMode = NoEdit }
    do! Env.askMediator |>> EventMediator.post (GameSessionNameChanged newName) newState.Config.ConfigId

    return newState
}

let acceptSessionNameEdit sessionName state model = monad {
    let! newState =
        match sessionName |> GameSessionName.create with
        | Ok newName -> doAcceptSessionNameEdit newName state
        | _ -> state |> Env.none

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
        { model with State = ConfiguringSession newState }, Cmd.none
    | None ->
        model, Cmd.none

let inputPlayerName playerName editedName state model =
    { model with State = ConfiguringSession { state with EditMode = EditPlayer (playerName, editedName) } }, Cmd.none

let private doAcceptPlayerNameEdit playerId newName state = monad {
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
    do! Env.askMediator |>> EventMediator.post (PlayerRenamed renameInfo) newState.Config.ConfigId

    return newState
}

let acceptPlayerNameEdit playerName editedName state model = monad {
    let! newState =
        match editedName |> PlayerName.create with
        | Ok newName -> doAcceptPlayerNameEdit playerName newName state
        | _ -> state |> Env.none

    return! model |> Flow.updateSession newState
}

let cancelEdit state model =
    { model with State = ConfiguringSession { state with EditMode = NoEdit } }, Cmd.none

let isEditedPlayerNameValid players playerId editedName =
    match editedName |> PlayerName.create with
    | Ok name -> players |> List.forall (fun (player : Player) -> player.Id = playerId || player.Name <> name)
    | _ -> false

let isEditedSessionNameValid = GameSessionName.create >> function Ok _ -> true | _ -> false

let removePlayer playerId state model = monad {
    let players = state.Config.ConfigPlayers |> List.filter (fun player -> player.Id <> playerId)
    let newState = { state with Config = { state.Config with ConfigPlayers = players } }
    
    do! Env.askMediator |>> EventMediator.post (PlayerRemoved playerId) newState.Config.ConfigId

    return! model |> Flow.updateSession newState
}
