namespace Chipper

type ChipError = ChipValueOutOfRange of int

type BetAmountError = BetAmoutOutOfRange of int

type PlayerNameError =
    | InvalidPlayerName of string

type PlayerListError =
    | DuplicatePlayerName of string

type GameSessionNameError =
    | EmptyGameSessionName
    | GameSessionNameTooLong of string

type BetRoundNumberError =
    | BetRoundNumberOutOfRange of int

type GameSessionError =
    | GamePlayersNumberOutOfRange of int
    | InvalidChipDistribution
    | InvalidBetRoundNumber of BetRoundNumberError

type GameError =
    | InvalidFirstPlayer of string

type DomainError =
    | ChipError of ChipError
    | BetAmountError of BetAmountError
    | PlayerNameError of PlayerNameError
    | PlayerListError of PlayerListError
    | GameSessionNameError of GameSessionNameError
    | BetRoundNumberError of BetRoundNumberError
    | GameSessionError of GameSessionError
    | GameError of GameError
