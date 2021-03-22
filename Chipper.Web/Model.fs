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

type ConfigSessionEditMode =
    | NoEdit
    | EditPlayer of PlayerId * string

type ConfigSessionState = {
    Config : GameSessionConfig
    EditMode : ConfigSessionEditMode
}

type PlayerRenameInfo = {
    HostName : PlayerName
    PlayerId : PlayerId
    NewName : PlayerName
}

type LocalState =
    | NoState
    | AddingSessionName of string * string
    | JoiningSession of JoiningPlayer
    | JoiningInvalidSession
    | AwaitingJoinConfirmation of ValidJoiningPlayer
    | AwaitingGameStart of ValidJoiningPlayer
    | AwaitingJoinRejected of ValidJoiningPlayer
    | AwaitingGameStartRemoved of ValidJoiningPlayer
    | AwaitingGameStartRenamed of ValidJoiningPlayer * PlayerRenameInfo
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

    let createJoinRequest { ValidGameSessionId = sessionId; ValidId = id; ValidName = name } =
        { PlayerId = id; Info = { GameSessionId = sessionId; PlayerName = name } }

    let tryCreateJoinInfo { GameSessionId = id; Name = name } =
        name |> playerName |> Result.map (fun playerName -> { GameSessionId = id; PlayerName = playerName })
