namespace Chipper.Web

open Chipper.Core.Domain

type Message =
    | SetPage of Page
    | SetInitialState of LocalState
    | SetModel of Model
    | StartGameSession
    | AddSessionName of GameSessionName
    | ConfigureGameSession
    | SetError of exn
