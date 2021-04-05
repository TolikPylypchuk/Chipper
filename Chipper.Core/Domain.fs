namespace Chipper.Core

open FSharpPlus

type ChipperError =
    | DomainError of DomainError
    | PersistenceError of PersistenceError
    | CustomError of string

[<RequireQualifiedAccess>]
module Domain =

    let private asDomainError result = result |> Result.mapError DomainError

    let chip = Chip.create >> Result.mapError ChipError >> asDomainError

    let betAmount = BetAmount.create >> Result.mapError BetAmountError >> asDomainError

    let playerName = PlayerName.create >> Result.mapError PlayerNameError >> asDomainError

    let gameSessionName = GameSessionName.create >> Result.mapError GameSessionNameError >> asDomainError

    let gameSession = GameSession.fromConfig >> Result.mapError GameSessionError >> asDomainError
