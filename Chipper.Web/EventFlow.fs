module Chipper.Web.EventFlow

open Elmish

let onPlayerAccessRequested state joinInfo model =
    let newState = { state with PlayerRequests = state.PlayerRequests @ [ joinInfo ] }
    { model with State = ConfiguringSession newState }, Cmd.none

let onPlayerAccepted player model =
    { model with State = AwaitingGameStart player }, Cmd.none

let onPlayerRejected player model =
    { model with State = AwaitingJoinRejected player }, Cmd.none
