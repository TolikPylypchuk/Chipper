namespace Chipper.Core

type ChipError = InvalidChipValue of int

type BetAmountError = InvalidBetAmout of int

type PlayerError = InvalidPlayerName of string

type GameSessionNameError =
    | EmptyGameSessionName
    | TooLongGameSessionName of string

type GameSessionError = InvalidGamePlayersNumber of int

type DomainError =
    | ChipError of ChipError list
    | BetAmountError of BetAmountError list
    | PlayerError of PlayerError list
    | GameSessionNameError of GameSessionNameError list
    | GameSessionError of GameSessionError list
