namespace Chipper.Web

open Chipper.Core
open Chipper.Core.Domain

type Debounced<'a> =
    | DebounceStart of 'a
    | DebounceEnd of 'a

type Message =
    | SetPage of Page
    | LoadLocalState of LocalState
    | RecoverLocalState
    | IgnoreLocalState
    | ClearLocalState
    | SetModel of Model
    | StartGameSession
    | InputSessionName of Debounced<string>
    | SaveSessionName
    | InputPlayerName of Debounced<string>
    | RequestAccess of PlayerJoinInfo
    | ConfigureGameSession
    | SetBettingType of BettingType
    | SetRaiseType of RaiseType
    | SetError of ChipperError

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
