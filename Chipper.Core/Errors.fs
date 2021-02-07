namespace Chipper.Core

type ChipError = InvalidChipValue of int

type BetAmountError = InvalidBetAmout of int

type PlayerError = InvalidPlayerName of string

type GameSessionError = InvalidGamePlayersNumber of int

type DomainError =
    | Chip of ChipError
    | BetAmount of BetAmountError
    | Player of PlayerError
    | GameSession of GameSessionError
