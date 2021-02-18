namespace Chipper.Web

open System

open Chipper.Core
open Chipper.Core.Domain

type Page =
    | HomePage
    | StartPage
    | InvitePage
    | JoinPage of SessionId : Guid
    | ConfigurePage
    | PlayPage
    | NotImplementedPage

type JoiningPlayer = {
    GameSessionId : GameSessionId
    GameSessionName : GameSessionName
    Name : string
}

type PlayerJoinInfo = {
    GameSessionId : GameSessionId
    PlayerName : PlayerName
}

type LocalState =
    | NoState
    | AddingSessionName of string
    | StartingSession of NewGameSession
    | JoiningSession of JoiningPlayer
    | JoiningInvalidSession
    | ConfiguringSession of GameSessionId

type Model = {
    Page : Page
    State : LocalState
    LocalState : LocalState option
    IsLoaded : bool
}

module Model =

    let simple page state = { Page = page; State = state; LocalState = None; IsLoaded = true }

    let canSaveSessionName name =
        match name |> gameSessionName with
        | Ok _ -> true
        | _ -> false

    let tryCreateJoinInfo { GameSessionId = id; Name = name } =
        name |> playerName |> Result.map (fun playerName -> { GameSessionId = id; PlayerName = playerName })
