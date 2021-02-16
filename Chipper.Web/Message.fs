namespace Chipper.Web

open Chipper.Core
open Chipper.Core.Domain

type Message =
    | SetPage of Page
    | SetInitialState of LocalState
    | SetModel of Model
    | StartGameSession
    | AddSessionName of GameSessionName
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
