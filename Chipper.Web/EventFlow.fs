module Chipper.Web.EventFlow

open FSharpPlus
open FSharpPlus.Data

open Chipper.Core

let private onPlayerAccessRequested state request model = monad {
    let config = { state.Config with ConfigPlayerRequests = state.Config.ConfigPlayerRequests @ [ request ] }
    let newState = { state with Config = config }
    let localState = ConfiguringSession newState

    do! Flow.setStateSimple localState
    return! { model with State = localState } |> Flow.updateSession newState
}

let private onPlayerAccepted player model : Flow<Model> = monad {
    let newState = AwaitingGameStart player
    do! Flow.setStateSimple newState
    return { model with State = newState }
}

let private onPlayerRejected player model : Flow<Model> = monad {
    do! Flow.clearStateSimple
    return { model with State = AwaitingJoinRejected player }
}

let private onPlayerRemoved player model : Flow<Model> = monad {
    do! Flow.clearStateSimple
    return { model with State = AwaitingGameStartRemoved player }
}

let private onPlayerRenamed player renameInfo model : Flow<Model> = monad {
    let renamedPlayer = { player with ValidName = renameInfo.NewName }
    do! Flow.setStateSimple <| AwaitingGameStart renamedPlayer
    return { model with State = AwaitingGameStartRenamed (renamedPlayer, renameInfo) }
}

let private onPlayerRequestCanceled state playerId model = monad {
    let newPlayers =
        state.Config.ConfigPlayers
        |> PlayerList.removePlayer playerId

    let newPlayerRequests =
        state.Config.ConfigPlayerRequests
        |> List.filter (fun request -> request.PlayerId <> playerId)

    let config = { state.Config with ConfigPlayers = newPlayers; ConfigPlayerRequests = newPlayerRequests }
    let newState = { state with Config = config }
    let localState = ConfiguringSession newState

    do! Flow.setStateSimple localState
    return! { model with State = localState } |> Flow.updateSession newState
}

let private onGameSessionNameChangedWhenJoiningSession player newName model =
    { model with State = JoiningSession { player with GameSessionName = newName } } |> pureFlow

let private onGameSessionNameChangedWhenAwaitingJoinConfirmation player newName model =
    { model with State = AwaitingJoinConfirmation { player with ValidGameSessionName = newName } } |> pureFlow

let private onGameSessionNameChangedWhenAwaitingJoinRejected player newName model =
    { model with State = AwaitingJoinRejected { player with ValidGameSessionName = newName } } |> pureFlow

let private onGameSessionNameChangedWhenAwaitingGameStart player newName model =
    { model with State = AwaitingGameStart { player with ValidGameSessionName = newName } } |> pureFlow

let private onGameSessionNameChangedWhenAwaitingGameStartRenamed player renameInfo newName model =
    let player = { player with ValidGameSessionName = newName }
    { model with State = AwaitingGameStartRenamed (player, renameInfo) } |> pureFlow

let private onGameSessionNameChangedWhenAwaitingGameStartRemoved player newName model =
    { model with State = AwaitingGameStartRemoved { player with ValidGameSessionName = newName } } |> pureFlow

let private onGameSessionNameChangedWhenJoinRequestCanceled newName model =
    { model with State = JoinRequestCanceled newName } |> pureFlow

let private onGameSessionStarted gameSession joiningPlayer model =
    let player =
        gameSession
        |> GameSession.players
        |> PlayerList.value
        |> NonEmptyList.toList
        |> List.filter (Player.id >> (=) joiningPlayer.ValidId)
        |> List.tryHead

    match player with
    | Some player -> { model with Page = PlayPage; State = Playing { GameSession = gameSession; Player = player } }
    | None -> model
    |> pureFlow

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

    | PlayerRequestCanceled id, ConfiguringSession state ->
        model |> onPlayerRequestCanceled state id

    | GameSessionNameChanged newName, JoiningSession player ->
        model |> onGameSessionNameChangedWhenJoiningSession player newName

    | GameSessionNameChanged newName, AwaitingJoinConfirmation player ->
        model |> onGameSessionNameChangedWhenAwaitingJoinConfirmation player newName

    | GameSessionNameChanged newName, AwaitingJoinRejected player ->
        model |> onGameSessionNameChangedWhenAwaitingJoinRejected player newName

    | GameSessionNameChanged newName, AwaitingGameStart player ->
        model |> onGameSessionNameChangedWhenAwaitingGameStart player newName

    | GameSessionNameChanged newName, AwaitingGameStartRenamed (player, renameInfo) ->
        model |> onGameSessionNameChangedWhenAwaitingGameStartRenamed player renameInfo newName

    | GameSessionNameChanged newName, AwaitingGameStartRemoved player ->
        model |> onGameSessionNameChangedWhenAwaitingGameStartRemoved player newName

    | GameSessionNameChanged newName, JoinRequestCanceled _ ->
        model |> onGameSessionNameChangedWhenJoinRequestCanceled newName

    | GameSessionStarted gameSession, AwaitingGameStart player ->
        model |> onGameSessionStarted gameSession player

    | _ ->
        model |> pureFlow
