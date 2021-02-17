namespace Chipper.Web

open Chipper.Core
open Chipper.Core.Domain

type Debounced<'a> =
    | DebounceStart of 'a
    | DebounceEnd of 'a

type Message =
    | SetPage of Page
    | SetInitialState of LocalState
    | SetModel of Model
    | StartGameSession
    | InputSessionName of Debounced<string>
    | SaveSessionName
    | InputPlayerName of Debounced<string>
    | RequestAccess of PlayerJoinInfo
    | ConfigureGameSession
    | SetError of ChipperError
    | SetException of exn

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
