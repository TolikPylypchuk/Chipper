module Chipper.Web.PlayerFlow

open FSharpPlus

open Elmish

open Chipper.Core.Domain

let inputPlayerNameWhenAddingSessionName name playerName model =
    { model with State = AddingSessionName (name, playerName) }, Cmd.none

let inputPlayerNameWhenJoiningSession id sessionName name model =
    let state = JoiningSession { GameSessionId = id; GameSessionName = sessionName; Name = name }
    { model with State = state }, Cmd.none
    
let private doRequestAccess player joinInfo = monad {
    let! mediator = Env.askMediator
    mediator |> EventMediator.post (PlayerAccessRequested joinInfo) player.ValidGameSessionId
    return AwaitingJoinConfirmation player
}

let requestAccess player joinInfo model = monad {
    let validPlayer = player |> ValidJoiningPlayer.create joinInfo
    let! newState = doRequestAccess validPlayer joinInfo
    let! loop = Flow.createEventLoop player.GameSessionId
    return { model with State = newState }, loop
}

let requestAccessAgain player joinInfo model = monad {
    let! newState = doRequestAccess player joinInfo
    return { model with State = newState }, Cmd.none
}

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

let inputNameWhenConfiguringSession playerName editedName state model =
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
