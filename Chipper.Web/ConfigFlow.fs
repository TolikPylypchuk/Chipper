module Chipper.Web.ConfigFlow

open FSharpPlus

open FsToolkit.ErrorHandling

open Elmish

open Chipper.Core
open Chipper.Core.Domain
open Chipper.Core.Persistence
open Chipper.Web

let private configureSession config model = monad {
    let! storage = Env.askStorage
    let! repo = Env.askRepo

    let result = asyncResult {
        do! repo |> updateSession (ConfigurableSession config)
        let state = ConfiguringSession { Config = config; PlayerRequests = []; EditMode = NoEdit }
        do! storage.SetState state
        return Message.setModel { model with Page = ConfigurePage; State = state }
    }

    return model, result |> Message.handleAsyncError |> Cmd.OfAsync.result
}

let setBettingType bettingType config =
    configureSession { config with ConfigBettingType = bettingType }

let setRaiseType raiseType config =
    configureSession { config with ConfigRaiseType = raiseType }

let editPlayerName playerName state model =
    let newState = { state with EditMode = ConfigSessionEditMode.Player (playerName, playerName |> PlayerName.value) }
    { model with State = ConfiguringSession newState }, Cmd.none

let acceptPlayerRequest playerName state model = monad {
    let newPlayer = { Name = playerName; Chips = [] }
    let playerRequests = state.PlayerRequests |> List.filter (fun joinInfo -> joinInfo.PlayerName <> playerName)

    let newState =
        { state with
            Config = { state.Config with ConfigPlayers = state.Config.ConfigPlayers @ [ newPlayer ] }
            PlayerRequests = playerRequests
        }

    do! Env.askMediator |>> EventMediator.post (PlayerAccepted playerName) newState.Config.ConfigId

    return { model with State = ConfiguringSession newState }, Cmd.none
}

let rejectPlayerRequest playerName state model = monad {
    let playerRequests = state.PlayerRequests |> List.filter (fun joinInfo -> joinInfo.PlayerName <> playerName)
    let newState = { state with PlayerRequests = playerRequests }
    
    do! Env.askMediator |>> EventMediator.post (PlayerRejected playerName) newState.Config.ConfigId

    return { model with State = ConfiguringSession newState }, Cmd.none
}

let inputPlayerName playerName editedName state model =
    { model with State = ConfiguringSession { state with EditMode = Player (playerName, editedName) } }, Cmd.none

let private doAcceptPlayerNameEdit playerName newName state = monad {
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

    do! Env.askMediator |>> EventMediator.post (PlayerRenamed (playerName, newName)) newState.Config.ConfigId

    return ConfiguringSession newState
}

let acceptPlayerNameEdit playerName editedName state model = monad {
    let! newState =
        match editedName |> PlayerName.create with
        | Ok newName -> doAcceptPlayerNameEdit playerName newName state
        | _ -> ConfiguringSession state |> Env.none

    return { model with State = newState }, Cmd.none
}

let cancelPlayerNameEdit state model =
    { model with State = ConfiguringSession { state with EditMode = NoEdit } }, Cmd.none

let isEditedPlayerNameValid players originalName editedName =
    match editedName |> PlayerName.create with
    | Ok name -> players |> List.forall (fun (player : Player) -> player.Name = originalName || player.Name <> name)
    | _ -> false

let removePlayer playerName state model = monad {
    let players = state.Config.ConfigPlayers |> List.filter (fun player -> player.Name <> playerName)
    let newState = { state with Config = { state.Config with ConfigPlayers = players } }
    
    do! Env.askMediator |>> EventMediator.post (PlayerRemoved playerName) newState.Config.ConfigId

    return { model with State = ConfiguringSession newState }, Cmd.none
}
