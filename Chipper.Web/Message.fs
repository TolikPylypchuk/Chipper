namespace Chipper.Web

type Message =
    | SetPage of Page
    | SetInitialState of LocalState
    | StartGameSession
    | ConfigureGameSession
    | SetError of exn
