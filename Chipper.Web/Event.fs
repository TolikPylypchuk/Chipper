namespace Chipper.Web

open Chipper.Core.Domain

type Event =
    | PlayerAccessRequested of PlayerJoinInfo
    | PlayerAccepted of PlayerName
    | PlayerRejected of PlayerName
    | PlayerRenamed of PlayerName * PlayerName
    | PlayerRemoved of PlayerName

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
