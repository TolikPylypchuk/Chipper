namespace Chipper.Web

open System

open Chipper.Core.Domain

type Page =
    | HomePage
    | StartPage
    | JoinPage of SessionId : Guid
    | ConfigureSessionPage
    | NotImplementedPage

type LocalState =
    | NotLoaded
    | NoState
    | StartingSession of GameSessionId
    | JoiningSession of GameSessionId
    | ConfiguringSession of GameSessionId

type Model = {
    Page : Page
    State : LocalState
}
