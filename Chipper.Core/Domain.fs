namespace Chipper.Core

open FSharpPlus

type ChipperError =
    | DomainError of DomainError
    | PersistenceError of PersistenceError
    | CustomError of string

[<RequireQualifiedAccess>]
module Domain =

    let private asDomainError error result = result |> Result.mapError (error >> DomainError)

    let chip = Chip.create >> asDomainError ChipError

    let betAmount = BetAmount.create >> asDomainError BetAmountError

    let playerName = PlayerName.create >> asDomainError PlayerNameError

    let editPlayerName playerId players = PlayerList.editPlayerName playerId players >> asDomainError PlayerListError

    let gameSessionName = GameSessionName.create >> asDomainError GameSessionNameError

    let gameSession = GameSession.fromConfig >> asDomainError GameSessionError
