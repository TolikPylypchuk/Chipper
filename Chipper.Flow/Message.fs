namespace Chipper

type GenericMessage =
    | NoMessage
    | SetPage of Page
    | SetError of ChipperError
    | LoadLocalState of LocalState
    | RecoverLocalState
    | IgnoreLocalState
    | ClearLocalState
    | SetModel of Model
    | ReceiveEvent of Event
    | CustomMessage of obj

type GameStartMessage =
    | StartGameSessionConfig
    | InputSessionName of string
    | InputPlayerName of string
    | SaveSessionName of GameSessionName * PlayerName
    | SessionSaved of GameSessionConfig
    | RequestAccess of PlayerJoinInfo
    | RequestAccessAgain of PlayerJoinRequest
    | AcceptRename
    | CancelRequest

type ConfigMessage =
    | EditSessionName
    | ConfigInputSessionName of string
    | EditPlayerName of PlayerId
    | ConfigInputPlayerName of string
    | AcceptSessionNameEdit of GameSessionName
    | AcceptPlayerNameEdit of ConfigPlayerList
    | CancelEdit
    | AcceptPlayerRequest of PlayerId
    | RejectPlayerRequest of PlayerId
    | RemovePlayer of PlayerId
    | MovePlayerUp of PlayerId
    | MovePlayerDown of PlayerId
    | SetChipEqualDistributionValue of Chip * int
    | InputBetRoundNumber of int
    | StartGameSession of GameSession

type Message =
    | GenericMessage of GenericMessage
    | GameStartMessage of GameStartMessage
    | ConfigMessage of ConfigMessage

[<RequireQualifiedAccess>]
module Message =

    let noMessage = NoMessage |> GenericMessage
    let setPage = SetPage >> GenericMessage
    let setError = SetError >> GenericMessage
    let loadLocalState = LoadLocalState >> GenericMessage
    let recoverLocalState = RecoverLocalState |> GenericMessage
    let ignoreLocalState = IgnoreLocalState |> GenericMessage
    let clearLocalState = ClearLocalState |> GenericMessage
    let setModel = SetModel >> GenericMessage
    let receiveEvent = ReceiveEvent >> GenericMessage
    let custom = CustomMessage >> GenericMessage

    let startGameSessionConfig = StartGameSessionConfig |> GameStartMessage
    let inputSessionName = InputSessionName >> GameStartMessage
    let inputPlayerName = InputPlayerName >> GameStartMessage
    let saveSessionName = SaveSessionName >> GameStartMessage
    let sessionSaved = SessionSaved >> GameStartMessage
    let requestAccess = RequestAccess >> GameStartMessage
    let requestAccessAgain = RequestAccessAgain >> GameStartMessage
    let acceptRename = AcceptRename |> GameStartMessage
    let cancelRequest = CancelRequest |> GameStartMessage

    let editSessionName = EditSessionName |> ConfigMessage
    let configInputSessionName = ConfigInputSessionName >> ConfigMessage
    let editPlayerName = EditPlayerName >> ConfigMessage
    let configInputPlayerName = ConfigInputPlayerName >> ConfigMessage
    let acceptSessionNameEdit = AcceptSessionNameEdit >> ConfigMessage
    let acceptPlayerNameEdit = AcceptPlayerNameEdit >> ConfigMessage
    let cancelEdit = CancelEdit |> ConfigMessage
    let acceptPlayerRequest = AcceptPlayerRequest >> ConfigMessage
    let rejectPlayerRequest = RejectPlayerRequest >> ConfigMessage
    let removePlayer = RemovePlayer >> ConfigMessage
    let movePlayerUp = MovePlayerUp >> ConfigMessage
    let movePlayerDown = MovePlayerDown >> ConfigMessage
    let setChipEqualDistributionValue chip num = SetChipEqualDistributionValue (chip, num) |> ConfigMessage
    let inputBetRoundNumber = InputBetRoundNumber >> ConfigMessage
    let startGameSession = StartGameSession >> ConfigMessage

    let handleError message =
        match message with
        | Ok message -> message
        | Error error -> setError error

    let handleAsyncError message = async {
        match! message with
        | Ok message -> return message
        | Error error -> return setError error
    }
