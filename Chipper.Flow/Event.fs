namespace Chipper

open Chipper

type Event =
    | PlayerAccessRequested of PlayerJoinRequest
    | PlayerAccepted of PlayerId
    | PlayerRejected of PlayerId
    | PlayerRenamed of PlayerRenameInfo
    | PlayerRemoved of PlayerId
    | PlayerRequestCanceled of PlayerId
    | GameSessionNameChanged of GameSessionName
    | GameSessionStarted of GameSession
    | GameStarted of Game * Player

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
