namespace Chipper.Web

open Bolero

type Message =
    | SetPage of Page
    | SetInitialState of LocalState
    | StartGameSession
    | ConfigureGameSession
    | SetError of exn
