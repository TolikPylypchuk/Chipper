namespace Chipper.Core

type ChipError = InvalidChipValue of int

type BetAmountError = InvalidBetAmout of int

type PlayerError = InvalidPlayerName of string

type GameSessionNameError =
    | EmptyGameSessionName
    | TooLongGameSessionName of string

type GameSessionError = InvalidGamePlayersNumber of int

type DomainError =
    | ChipError of ChipError
    | BetAmountError of BetAmountError
    | PlayerError of PlayerError
    | GameSessionNameError of GameSessionNameError
    | GameSessionError of GameSessionError
