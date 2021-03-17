module Chipper.Web.EventFlow

open FSharpPlus

open Elmish

open Chipper.Core.Domain

let onPlayerAccessRequested state joinInfo model = monad {
    let newState = ConfiguringSession { state with PlayerRequests = state.PlayerRequests @ [ joinInfo ] }
    do! Flow.setStateSimple newState
    return { model with State = newState }, Cmd.none
}

let onPlayerAccepted player model = monad {
    let newState = AwaitingGameStart player
    do! Flow.setStateSimple newState
    return { model with State = newState }, Cmd.none
}

let onPlayerRejected player model = monad {
    do! Flow.clearStateSimple
    return { model with State = AwaitingJoinRejected player }, Cmd.none
}

let onPlayerRemoved player model = monad {
    do! Flow.clearStateSimple
    return { model with State = AwaitingGameStartRemoved player }, Cmd.none
}

let onPlayerRenamed player renameInfo model = monad {
    let renamedPlayer = { player with ValidName = renameInfo.NewName }
    do! Flow.setStateSimple <| AwaitingGameStart renamedPlayer
    return { model with State = AwaitingGameStartRenamed (renamedPlayer, renameInfo) }, Cmd.none
}

let receiveEvent event model =
    match event, model.State with
    | PlayerAccessRequested joinInfo, ConfiguringSession state ->
        model |> onPlayerAccessRequested state joinInfo

    | PlayerAccepted playerName, AwaitingJoinConfirmation player when player.ValidName = playerName ->
        model |> onPlayerAccepted player

    | PlayerRejected playerName, AwaitingJoinConfirmation player when player.ValidName = playerName ->
        model |> onPlayerRejected player

    | PlayerRenamed renameInfo, AwaitingGameStart player when player.ValidName = renameInfo.OldName ->
        model |> onPlayerRenamed player renameInfo

    | PlayerRemoved playerName, AwaitingGameStart player when player.ValidName = playerName ->
        model |> onPlayerRemoved player

    | _ ->
        model |> Flow.doNothing |> Env.none
