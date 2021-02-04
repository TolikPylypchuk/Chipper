namespace Chipper.Core

open System

open FSharpx.Collections

open FSharpPlus

type Chip = private Chip of int

type Pot = Pot of Chip list

type Bet = Bet of NonEmptyList<Chip>

type Move =
    | Check
    | Call of Bet
    | Raise of Bet
    | Fold

type Player = private {
    Name : string
    Chips : Chip list
}

type PlayerMove = {
    Player : Player
    Move : Move
}

type BettingRoundName =
    | PreFlop
    | Flop
    | Turn
    | River

type BettingRound = private {
    SmallBlind : int
    BigBlind : int
    MinimumRaise : int
    Moves : PlayerMove list
}

type GameId = GameId of Guid

type GameType =
    | Limit
    | NoLimit
    | PotLimit

type Game = private {
    Id : GameId
    Type : GameType
    Players : NonEmptyList<Player>
    Host : Player
    Rounds : Map<BettingRoundName, BettingRound>
}

[<RequireQualifiedAccess>]
module Chip =

    let create value =
        if value > 0 && value <= 100_000_000
        then Chip value |> Ok
        else [ InvalidChipValue value ] |> Error

    let value (Chip chip) = chip

    let addChips chips player = { player with Chips = player.Chips @ chips |> List.sort }

    let removeChips chips player = { player with Chips = player.Chips |> List.except chips }

[<RequireQualifiedAccess>]
module Player =

    let create name =
        if (not <| String.IsNullOrEmpty(name)) && name.Length <= 50
        then { Name = name; Chips = [] } |> Ok
        else [ InvalidPlayerName name ] |> Error

    let name player = player.Name

    let chips player = player.Chips

[<RequireQualifiedAccess>]
module BettingRound =

    let private validateSmallBlind smallBlind =
        if smallBlind >= 1 && smallBlind <= 50_000_000
        then smallBlind |> Ok
        else [ SmallBlindInvalid smallBlind ] |> Error

    let private validateBigBlind bigBlind =
        if bigBlind >= 1 && bigBlind <= 100_000_000
        then bigBlind |> Ok
        else [ BigBlindInvalid bigBlind ] |> Error

    let private validateMinimumRaise raise =
        if raise >= 1 && raise <= 100_000_000
        then raise |> Ok
        else [ MinimumRaiseInvalid raise ] |> Error

    let private create' smallBlind bigBlind minimumRaise =
        { SmallBlind = smallBlind; BigBlind = bigBlind; MinimumRaise = minimumRaise; Moves = [] }

    let create smallBlind bigBlind minimumRaise =
        create' <!> validateSmallBlind smallBlind <*> validateBigBlind bigBlind <*> validateMinimumRaise minimumRaise

    let smallBlind round = round.SmallBlind

    let bigBlind round = round.BigBlind

    let minimumRaise round = round.MinimumRaise

    let moves round = round.Moves

    let addMove move round = { round with Moves = round.Moves @ [ move ] }

[<RequireQualifiedAccess>]
module Game =
    
    let private validatePlayersNumber players =
        let numPlayers = players |> NonEmptyList.length
        if numPlayers >= 2 && numPlayers <= 23
        then players |> Ok
        else [ InvalidGamePlayersNumber numPlayers ] |> Error

    let private validateHost players host =
        if players |> NonEmptyList.toList |> List.contains host
        then host |> Ok
        else [ GameHostIsNotAPlayer ] |> Error

    let private create' id type' players host =
        { Id = id; Type = type'; Players = players; Host = host; Rounds = Map.empty }

    let create id type' players host =
        create' id type' <!> validatePlayersNumber players <*> validateHost players host

    let type' game = game.Type

    let players game = game.Players

    let host game = game.Host

[<AutoOpen>]
module Patterns =

    let (|Chip|) (Chip chip) = chip
