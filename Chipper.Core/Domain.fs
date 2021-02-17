module Chipper.Core.Domain

open System
open FSharpx.Collections

type Chip = private Chip of int

type Pot = Pot of Chip list

type BetAmount = private BetAmount of int

type Bet = Bet of NonEmptyList<Chip>

type Move =
    | Check
    | Call of Bet
    | Raise of Bet
    | Fold

type PlayerName = private PlayerName of string

type Player = {
    Name : PlayerName
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

type GameSessionName = private GameSessionName of string

type NewGameSession = {
    Id : GameSessionId
    Name : GameSessionName
    Date : DateTime
}

type GameSessionConfig = {
    Id : GameSessionId
    Name : GameSessionName
    Date : DateTime
    Players : NonEmptyList<Player>
    RaiseType : RaiseType
    BettingType : BettingType
}

type GameSession = private {
    Config : GameSessionConfig
    Games : Game list
}

[<RequireQualifiedAccess>]
module Chip =

    let create value =
        if value > 0 && value <= 100_000_000
        then Chip value |> Ok
        else InvalidChipValue value |> Error

    let value (Chip chip) = chip

[<RequireQualifiedAccess>]
module BetAmount =

    let create amount =
        if amount > 0
        then BetAmount amount |> Ok
        else InvalidBetAmout amount |> Error

    let value (BetAmount amount) = amount

[<RequireQualifiedAccess>]
module PlayerName =

    let create name =
        if (not <| String.IsNullOrEmpty(name)) && name.Length <= 50
        then PlayerName name |> Ok
        else InvalidPlayerName name |> Error

    let value (PlayerName name) = name

[<RequireQualifiedAccess>]
module Player =

    let chipCounts player = player.Chips |> List.countBy id
    
    let addChips chips player = { player with Chips = player.Chips @ chips |> List.sort }

    let removeChips chips player = { player with Chips = player.Chips |> List.except chips }
    
[<RequireQualifiedAccess>]
module GameSessionName =
    
    let create name =
        if String.IsNullOrWhiteSpace(name) then
            EmptyGameSessionName |> Error
        else
            let name = name.Trim()
            let nameLength = name |> String.length
            if nameLength <= 50
            then GameSessionName name |> Ok
            else TooLongGameSessionName name |> Error
    
    let value (GameSessionName name) = name
    
[<RequireQualifiedAccess>]
module GameSession =

    let fromConfig sessionConfig =
        let numPlayers = sessionConfig.Players |> NonEmptyList.length
        if numPlayers > 1 && numPlayers <= 20
        then { Config = sessionConfig; Games = [] } |> Ok
        else InvalidGamePlayersNumber numPlayers |> Error

    let config session = session.Config
    let id session = session.Config.Id
    let players session = session.Config.Players
    let raiseType session = session.Config.RaiseType
    let bettingsType session = session.Config.BettingType
    let games session = session.Games

[<AutoOpen>]
module Patterns =

    let (|Chip|) (Chip chip) = chip
    
    let (|BetAmount|) (BetAmount amount) = amount
    
    let (|GameSessionName|) (GameSessionName name) = name

    let (|PlayerName|) (PlayerName name) = name
