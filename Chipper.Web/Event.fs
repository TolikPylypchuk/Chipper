namespace Chipper.Web

open System

open Chipper.Core.Domain

type Event =
    | PlayerAccessRequest of PlayerJoinInfo

type GameSessionEvent = {
    GameSessionId : GameSessionId
    Event : Event
}

type EventMediator = {
    Post : GameSessionEvent -> unit
    Subscribe : GameSessionId -> (Event -> unit) -> IDisposable
}

[<RequireQualifiedAccess>]
module EventMediator =

    let post event id mediator =
        mediator.Post { GameSessionId = id; Event = event }

    let subscribe id callback mediator =
        mediator.Subscribe id callback
