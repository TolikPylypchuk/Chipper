namespace Chipper.Web

open System

open Chipper.Core
open Chipper.Core.Domain

type Page =
    | HomePage
    | StartPage
    | JoinPage of SessionId : Guid
    | ConfigurePage
    | PlayPage
    | NotImplementedPage

type JoiningPlayer = {
    GameSessionId : GameSessionId
    GameSessionName : GameSessionName
    Name : string
}

type ValidJoiningPlayer = {
    ValidGameSessionId : GameSessionId
    ValidGameSessionName : GameSessionName
    ValidName : PlayerName
}

type PlayerJoinInfo = {
    GameSessionId : GameSessionId
    PlayerName : PlayerName
}

type ConfigSessionEditMode =
    | NoEdit
    | Player of PlayerName * string

type ConfigSessionState = {
    Config : GameSessionConfig
    PlayerRequests : PlayerJoinInfo list
    EditMode : ConfigSessionEditMode
}

type LocalState =
    | NoState
    | AddingSessionName of string * string
    | JoiningSession of JoiningPlayer
    | JoiningInvalidSession
    | AwaitingJoinConfirmation of ValidJoiningPlayer
    | AwaitingGameStart of ValidJoiningPlayer
    | AwaitingJoinRejected of ValidJoiningPlayer
    | ConfiguringSession of ConfigSessionState

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
        
    let canSavePlayerName name =
        match name |> playerName with
        | Ok _ -> true
        | _ -> false

    let createJoinInfo { ValidGameSessionId = id; ValidName = name } =
        { GameSessionId = id; PlayerName = name; }

    let tryCreateJoinInfo { GameSessionId = id; Name = name } =
        name |> playerName |> Result.map (fun playerName -> { GameSessionId = id; PlayerName = playerName })
