namespace Chipper.Web

open System
open System.Reactive.Subjects

open Chipper.Core.Domain

type Event =
    | PlayerAccessRequest of PlayerJoinInfo

type GameSessionEvent = {
    GameSessionId : GameSessionId
    Event : Event
}

type IEventMediator =
    abstract member Post : GameSessionEvent -> unit
    abstract member Subscribe : GameSessionId -> (Event -> unit) -> unit

[<RequireQualifiedAccess>]
module EventMediator =

    let post event id (mediator : IEventMediator) =
        mediator.Post { GameSessionId = id; Event = event }

    let subscribe id callback (mediator : IEventMediator) =
        mediator.Subscribe id callback
