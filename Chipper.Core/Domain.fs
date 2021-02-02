namespace Chipper.Core

open System

open FSharpx.Collections

type Chip = private Chip of int

type Move =
    | Check
    | Call of NonEmptyList<Chip>
    | Raise of NonEmptyList<Chip>
    | Fold

type Player = private {
    Name : string
    Chips : Chip list
}

type Game = {
    Players : NonEmptyList<Player>
    Pot : Chip list
    SidePots : (NonEmptyList<Player> * NonEmptyList<Chip>) list
}

[<RequireQualifiedAccess>]
module Chip =

    let create value =
        if value > 0 && value <= 100_000_000
        then Chip value |> Ok
        else InvalidChipValue value |> Error

    let value (Chip chip) = chip

[<RequireQualifiedAccess>]
module Player =

    let create name =
        if (not <| String.IsNullOrEmpty(name)) && name.Length <= 50
        then { Name = name; Chips = [] } |> Ok
        else InvalidPlayerName name |> Error

    let name player = player.Name

    let chips player = player.Chips

[<AutoOpen>]
module Patterns =

    let (|Chip|) (Chip chip) = chip
