namespace Chipper.Web

open System

open Chipper.Core

type Page =
    | HomePage
    | StartPage of sessionId : Guid
    | JoinPage of sessionId : Guid
    | ConfigureSessionPage of sessionId : Guid
    | NotImplementedPage

type ModelState =
    | NoState
    | StartingSession of GameSessionId
    | JoiningSession of GameSessionId
    | ConfiguringSession of GameSessionId

type Model = {
    Page : Page
    State : ModelState
}
