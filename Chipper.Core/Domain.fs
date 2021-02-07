namespace Chipper.Core

open System

open FSharpx.Collections

open FSharpPlus

type Chip = private Chip of int

type Pot = Pot of Chip list

type BetAmount = private BetAmount of int

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

type BettingRound = {
    MinimumRaise : BetAmount
    Moves : Move list
}

type RaiseType =
    | Limit
    | NoLimit
    | PotLimit

type BlindBets = {
    SmallBlind : BetAmount
    BigBlind : BetAmount
}

type AnteBet = AnteBet of BetAmount

type BettingType =
    | Blinds of BlindBets
    | Ante of AnteBet
    
type GameId = GameId of Guid
    
type Game = {
    Id : GameId
    Rounds : BettingRound list
}

type GameSessionId = GameSessionId of Guid

type GameSession = private {
    Id : GameSessionId
    Players : NonEmptyList<Player>
    RaiseType : RaiseType
    BettingType : BettingType
    Games : Game list
}

[<RequireQualifiedAccess>]
module Chip =

    let create value =
        if value > 0 && value <= 100_000_000
        then Chip value |> Ok
        else [ InvalidChipValue value ] |> Error

    let value (Chip chip) = chip

[<RequireQualifiedAccess>]
module BetAmount =

    let create amount =
        if amount > 0
        then BetAmount amount |> Ok
        else [ InvalidBetAmout amount ] |> Error

    let value (BetAmount amount) = amount

[<RequireQualifiedAccess>]
module Player =

    let create name =
        if (not <| String.IsNullOrEmpty(name)) && name.Length <= 50
        then { Name = name; Chips = [] } |> Ok
        else [ InvalidPlayerName name ] |> Error

    let name player = player.Name

    let chips player = player.Chips

    let chipCounts player = player.Chips |> List.countBy id
    
    let addChips chips player = { player with Chips = player.Chips @ chips |> List.sort }

    let removeChips chips player = { player with Chips = player.Chips |> List.except chips }

[<RequireQualifiedAccess>]
module GameSession =

    let create id players raiseType bettingType =
        let numPlayers = players |> NonEmptyList.length
        if numPlayers > 1
        then { Id = id; Players = players; RaiseType = raiseType; BettingType = bettingType; Games = [] } |> Ok
        else [ InvalidGamePlayersNumber numPlayers ] |> Error

    let id session = session.Id

    let players session = session.Players

    let raiseType session = session.RaiseType

    let bettingType session = session.BettingType

    let games session = session.Games

[<AutoOpen>]
module Patterns =

    let (|Chip|) (Chip chip) = chip
    
    let (|BetAmount|) (BetAmount amount) = amount
