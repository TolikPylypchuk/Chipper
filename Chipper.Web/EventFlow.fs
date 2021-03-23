module Chipper.Web.EventFlow

open FSharpPlus

open Elmish

open Chipper.Core.Domain

let onPlayerAccessRequested state request model = monad {
    let config = { state.Config with ConfigPlayerRequests = state.Config.ConfigPlayerRequests @ [ request ] }
    let newState = { state with Config = config }
    let localState = ConfiguringSession newState

    do! Flow.setStateSimple localState
    return! { model with State = localState } |> Flow.updateSession newState
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
    | PlayerAccessRequested request, ConfiguringSession state ->
        model |> onPlayerAccessRequested state request

    | PlayerAccepted playerId, AwaitingJoinConfirmation player when player.ValidId = playerId ->
        model |> onPlayerAccepted player

    | PlayerRejected playerId, AwaitingJoinConfirmation player when player.ValidId = playerId ->
        model |> onPlayerRejected player

    | PlayerRenamed renameInfo, (AwaitingGameStart player | AwaitingGameStartRenamed (player, _))
        when player.ValidId = renameInfo.PlayerId ->
        model |> onPlayerRenamed player renameInfo
        
    | PlayerRemoved playerId, (AwaitingGameStart player | AwaitingGameStartRenamed (player, _))
        when player.ValidId = playerId ->
        model |> onPlayerRemoved player
        
    | _ ->
        model |> Flow.doNothing |> Env.none
