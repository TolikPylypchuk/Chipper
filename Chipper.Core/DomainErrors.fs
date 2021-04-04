namespace Chipper.Core

type ChipError = InvalidChipValue of int

type BetAmountError = InvalidBetAmout of int

type PlayerNameError = InvalidPlayerName of string

type GameSessionNameError =
    | EmptyGameSessionName
    | TooLongGameSessionName of string

type GameSessionError =
    | InvalidGamePlayersNumber of int
    | InvalidChipDistribution

type DomainError =
    | ChipError of ChipError
    | BetAmountError of BetAmountError
    | PlayerNameError of PlayerNameError
    | GameSessionNameError of GameSessionNameError
    | GameSessionError of GameSessionError
