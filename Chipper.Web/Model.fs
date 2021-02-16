namespace Chipper.Web

open System

open Chipper.Core.Domain

type Page =
    | HomePage
    | StartPage
    | InvitePage
    | JoinPage of SessionId : Guid
    | ConfigurePage
    | PlayPage
    | NotImplementedPage

type LocalState =
    | NotLoaded
    | NoState
    | StartingSession of NewGameSession
    | JoiningSession of GameSessionId
    | ConfiguringSession of GameSessionId

type Model = {
    Page : Page
    State : LocalState
}
