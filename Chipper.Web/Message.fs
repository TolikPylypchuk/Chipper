namespace Chipper.Web

open Chipper.Core

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
