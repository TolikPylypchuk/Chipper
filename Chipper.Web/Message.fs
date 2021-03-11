namespace Chipper.Web

open Chipper.Core
open Chipper.Core.Domain

type Message =
    | SetPage of Page
    | SetError of ChipperError
    | LoadLocalState of LocalState
    | RecoverLocalState
    | IgnoreLocalState
    | ClearLocalState
    | SetModel of Model
    | ReceiveEvent of Event
    | StartGameSession
    | InputSessionName of string
    | SaveSessionName
    | SessionSaved of GameSessionConfig
    | InputPlayerName of string
    | RequestAccess of PlayerJoinInfo
    | SetBettingType of BettingType
    | SetRaiseType of RaiseType
    | EditPlayerName of PlayerName
    | AcceptEdit
    | CancelEdit

[<AutoOpen>]
module MessageUtil =

    let handleMessageError message =
        match message with
        | Ok message -> message
        | Error error -> SetError error

    let handleAsyncMessageError message = async {
        match! message with
        | Ok message -> return message
        | Error error -> return SetError error
    }
