namespace Chipper.Core

type ChipError = InvalidChipValue of int

type PlayerError = InvalidPlayerName of string

type BettingRoundError =
    | SmallBlindInvalid of int
    | BigBlindInvalid of int
    | MinimumRaiseInvalid of int

type GameError =
    | GameHostIsNotAPlayer
    | InvalidGamePlayersNumber of int

type DomainError =
    | Chip of ChipError
    | Player of PlayerError
    | BettingRound of BettingRoundError
    | Game of GameError
