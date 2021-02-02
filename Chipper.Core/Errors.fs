namespace Chipper.Core

type DomainError =
    | InvalidChipValue of int
    | InvalidPlayerName of string
