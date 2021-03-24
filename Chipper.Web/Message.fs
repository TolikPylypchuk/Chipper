namespace Chipper.Web

open Chipper.Core
open Chipper.Core.Domain

type GenericMessage =
    | SetPage of Page
    | SetError of ChipperError
    | LoadLocalState of LocalState
    | RecoverLocalState
    | IgnoreLocalState
    | ClearLocalState
    | SetModel of Model
    | ReceiveEvent of Event

type GameStartMessage =
    | StartGameSession
    | InputSessionName of string
    | InputPlayerName of string
    | SaveSessionName
    | SessionSaved of GameSessionConfig
    | RequestAccess of PlayerJoinInfo
    | RequestAccessAgain of PlayerJoinRequest
    | AcceptRename
    | CancelRequest

type ConfigMessage =
    | SetBettingType of BettingType
    | SetRaiseType of RaiseType
    | EditSessionName
    | ConfigInputSessionName of string
    | EditPlayerName of PlayerId
    | ConfigInputPlayerName of string
    | AcceptEdit
    | CancelEdit
    | AcceptPlayerRequest of PlayerId
    | RejectPlayerRequest of PlayerId
    | RemovePlayer of PlayerId

type Message =
    | GenericMessage of GenericMessage
    | GameStartMessage of GameStartMessage
    | ConfigMessage of ConfigMessage

[<RequireQualifiedAccess>]
module Message =

    let setPage = SetPage >> GenericMessage
    let setError = SetError >> GenericMessage
    let loadLocalState = LoadLocalState >> GenericMessage
    let recoverLocalState = RecoverLocalState |> GenericMessage
    let ignoreLocalState = IgnoreLocalState |> GenericMessage
    let clearLocalState = ClearLocalState |> GenericMessage
    let setModel = SetModel >> GenericMessage
    let receiveEvent = ReceiveEvent >> GenericMessage

    let startGameSession = StartGameSession |> GameStartMessage
    let inputSessionName = InputSessionName >> GameStartMessage
    let inputPlayerName = InputPlayerName >> GameStartMessage
    let saveSessionName = SaveSessionName |> GameStartMessage
    let sessionSaved = SessionSaved >> GameStartMessage
    let requestAccess = RequestAccess >> GameStartMessage
    let requestAccessAgain = RequestAccessAgain >> GameStartMessage
    let acceptRename = AcceptRename |> GameStartMessage
    let cancelRequest = CancelRequest |> GameStartMessage

    let setBettingType = SetBettingType >> ConfigMessage
    let setRaiseType = SetRaiseType >> ConfigMessage
    let editSessionName = EditSessionName |> ConfigMessage
    let configInputSessionName = ConfigInputSessionName >> ConfigMessage
    let editPlayerName = EditPlayerName >> ConfigMessage
    let configInputPlayerName = ConfigInputPlayerName >> ConfigMessage
    let acceptEdit = AcceptEdit |> ConfigMessage
    let cancelEdit = CancelEdit |> ConfigMessage
    let acceptPlayerRequest = AcceptPlayerRequest >> ConfigMessage
    let rejectPlayerRequest = RejectPlayerRequest >> ConfigMessage
    let removePlayer = RemovePlayer >> ConfigMessage

    let handleError message =
        match message with
        | Ok message -> message
        | Error error -> setError error

    let handleAsyncError message = async {
        match! message with
        | Ok message -> return message
        | Error error -> return setError error
    }
