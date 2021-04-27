namespace Chipper.Web

open System
open FSharpPlus

open Chipper.Core

type AppSettings = {
    UrlRoot : string
}

type Page =
    | HomePage
    | StartPage
    | JoinPage of SessionId : Guid
    | ConfigurePage
    | PlayPage
    | NotImplementedPage

type AddSessionState = {
    SessionName : string
    HostName : string
    Target : Result<GameSessionName * PlayerName, ChipperError>
}

type JoiningPlayer = {
    GameSessionId : GameSessionId
    GameSessionName : GameSessionName
    Name : string
    Target : Result<PlayerJoinInfo, ChipperError>
}

type ValidJoiningPlayer = {
    ValidGameSessionId : GameSessionId
    ValidGameSessionName : GameSessionName
    ValidId : PlayerId
    ValidName : PlayerName
}

type EditedPlayerName = {
    Id : PlayerId
    Name : string
    Target : Result<ConfigPlayerList, ChipperError>
}

type EditedSessionName = {
    Name : string
    Target : Result<GameSessionName, ChipperError>
}

type ConfigSessionEditMode =
    | NoEdit
    | EditSession of EditedSessionName
    | EditPlayer of EditedPlayerName

type ConfigSessionState = {
    Config : GameSessionConfig
    EditMode : ConfigSessionEditMode
    Target : Result<GameSession, GameSessionError>
}

type PlayerRenameInfo = {
    HostName : PlayerName
    PlayerId : PlayerId
    NewName : PlayerName
}

type GameSessionState = {
    GameSession : GameSession
    Player : Player
}

type LocalState =
    | NoState
    | AddingSessionName of AddSessionState
    | JoiningSession of JoiningPlayer
    | JoiningInvalidSession
    | AwaitingJoinConfirmation of ValidJoiningPlayer
    | AwaitingGameStart of ValidJoiningPlayer
    | AwaitingJoinRejected of ValidJoiningPlayer
    | AwaitingGameStartRemoved of ValidJoiningPlayer
    | AwaitingGameStartRenamed of ValidJoiningPlayer * PlayerRenameInfo
    | JoinRequestCanceled of GameSessionName
    | ConfiguringSession of ConfigSessionState
    | Playing of GameSessionState

type Model = {
    Page : Page
    State : LocalState
    LocalState : LocalState option
    IsLoaded : bool
}

[<RequireQualifiedAccess>]
module Model =

    let simple page state = { Page = page; State = state; LocalState = None; IsLoaded = true }

    let createAddSessionState sessionName hostName =
        let target = tuple2 <!> (sessionName |> Domain.gameSessionName) <*> (hostName |> Domain.playerName)
        { HostName = hostName; SessionName = sessionName; Target = target }

    let addSessionStateFromConfig config =
        {
            SessionName = config.ConfigName |> GameSessionName.value
            HostName = (config.ConfigPlayers |> PlayerList.configHost).Name |> PlayerName.value
            Target = Ok (config.ConfigName, (config.ConfigPlayers |> PlayerList.configHost).Name)
        }

    let tryCreateJoiningPlayer id name =
        name |> Domain.playerName |>> fun name -> { GameSessionId = id; PlayerName = name }

    let createValidJoiningPlayer (joinRequest : PlayerJoinRequest) (player : JoiningPlayer) =
        {
            ValidGameSessionId = player.GameSessionId
            ValidGameSessionName = player.GameSessionName
            ValidId = joinRequest.PlayerId
            ValidName = joinRequest.Info.PlayerName
        }

    let createJoinRequest { ValidGameSessionId = sessionId; ValidId = id; ValidName = name } =
        { PlayerId = id; Info = { GameSessionId = sessionId; PlayerName = name } }
