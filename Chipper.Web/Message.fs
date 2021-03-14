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
    | SaveSessionName
    | SessionSaved of GameSessionConfig

type PlayerMessage =
    | InputPlayerName of string
    | RequestAccess of PlayerJoinInfo
    | EditPlayerName of PlayerName
    | AcceptPlayerNameEdit
    | CancelPlayerNameEdit
    | AcceptPlayerRequest of PlayerName
    | RejectPlayerRequest of PlayerName

type ConfigMessage =
    | SetBettingType of BettingType
    | SetRaiseType of RaiseType

type Message =
    | GenericMessage of GenericMessage
    | GameStartMessage of GameStartMessage
    | PlayerMessage of PlayerMessage
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
    let saveSessionName = SaveSessionName |> GameStartMessage
    let sessionSaved = SessionSaved >> GameStartMessage

    let inputPlayerName = InputPlayerName >> PlayerMessage
    let requestAccess = RequestAccess >> PlayerMessage
    let editPlayerName = EditPlayerName >> PlayerMessage
    let acceptPlayerNameEdit = AcceptPlayerNameEdit |> PlayerMessage
    let cancelPlayerNameEdit = CancelPlayerNameEdit |> PlayerMessage
    let acceptPlayerRequest = AcceptPlayerRequest >> PlayerMessage
    let rejectPlayerRequest = RejectPlayerRequest >> PlayerMessage

    let setBettingType = SetBettingType >> ConfigMessage
    let setRaiseType = SetRaiseType >> ConfigMessage

    let handleError message =
        match message with
        | Ok message -> message
        | Error error -> setError error

    let handleAsyncError message = async {
        match! message with
        | Ok message -> return message
        | Error error -> return setError error
    }
