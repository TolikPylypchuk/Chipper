namespace Chipper.Core

open System

open FSharpPlus
open FSharpPlus.Data

type Chip = private Chip of int

type Pot = Pot of Chip list

type BetAmount = private BetAmount of int

type Bet = Bet of NonEmptyList<Chip>

type Move =
    | Check
    | Call of Bet
    | Raise of Bet
    | Fold

type PlayerId = PlayerId of Guid

type PlayerName = private PlayerName of string

type Player = {
    Id : PlayerId
    Name : PlayerName
    Chips : Chip list
}

type BettingRound = {
    MinimumRaise : BetAmount
    Moves : Move list
}

type GameId = GameId of Guid

type Game = {
    Id : GameId
    Rounds : BettingRound list
}

type GameSessionId = GameSessionId of Guid

type GameSessionName = private GameSessionName of string

type PlayerJoinInfo = {
    GameSessionId : GameSessionId
    PlayerName : PlayerName
}

type PlayerJoinRequest = {
    PlayerId : PlayerId
    Info : PlayerJoinInfo
}

type ChipDistribution = EqualChipDitribution of Map<Chip, int>

type GameSessionConfig = {
    ConfigId : GameSessionId
    ConfigName : GameSessionName
    ConfigPlayerRequests : PlayerJoinRequest list
    ConfigDate : DateTime
    ConfigHost : Player
    ConfigPlayers : Player list
    ConfigChipDistribution : ChipDistribution
}

type GameSession = private {
    Id : GameSessionId
    Name : GameSessionName
    Date : DateTime
    Players : NonEmptyList<Player>
    Games : Game list
}

[<RequireQualifiedAccess>]
module Chip =

    let create value =
        if value > 0 && value <= 1_000_000
        then Chip value |> Ok
        else InvalidChipValue value |> Error

    let value (Chip chip) = chip

    let one () = Chip 1
    let five () = Chip 5
    let ten () = Chip 10
    let twentyFive () = Chip 25
    let oneHundred () = Chip 100

[<RequireQualifiedAccess>]
module BetAmount =

    let create amount =
        if amount > 0 && amount <= 2_000_000_000
        then BetAmount amount |> Ok
        else InvalidBetAmout amount |> Error

    let value (BetAmount amount) = amount

[<RequireQualifiedAccess>]
module PlayerName =

    let private maxLength = 50

    let create name =
        if (not <| String.IsNullOrEmpty(name)) && name.Length <= maxLength
        then PlayerName name |> Ok
        else InvalidPlayerName name |> Error

    let value (PlayerName name) = name

    let theGame = PlayerName <| nameof Chipper

    let appendNumber num (PlayerName playerName) =
        let playerNameAndNumber = $"{playerName} {num}"

        if playerNameAndNumber.Length <= maxLength then
            playerNameAndNumber |> PlayerName
        else
            let lengthToCut = playerNameAndNumber.Length - maxLength
            $"{playerName.Remove(playerName.Length - lengthToCut)} {num}" |> PlayerName

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

    let defaultConfig id name date hostId hostName =
        {
            ConfigId = id
            ConfigName = name
            ConfigDate = date
            ConfigHost = { Id = hostId; Name = hostName; Chips = [] }
            ConfigPlayers = []
            ConfigPlayerRequests = []
            ConfigChipDistribution =
                [
                    Chip.one (), 0
                    Chip.five (), 0
                    Chip.ten (), 0
                    Chip.twentyFive (), 0
                    Chip.oneHundred (), 0
                ]
                |> Map.ofList
                |> EqualChipDitribution
        }

    let private assignChips (EqualChipDitribution chips) players =
        if chips |> Map.toList |> List.map snd |> List.exists ((<) 0) then
            Error InvalidChipDistribution
        else
            let chipsPerPlayer =
                chips
                |> Map.toList
                |> List.collect (fun (chip, amount) -> chip |> List.replicate amount)

            players
            |> NonEmptyList.map (fun player -> { player with Chips = chipsPerPlayer })
            |> Ok

    let fromConfig config =
        let numPlayers = config.ConfigPlayers |> List.length
        if numPlayers > 0 && numPlayers <= 20 then
            let allPlayers = NonEmptyList.create config.ConfigHost config.ConfigPlayers
            allPlayers |> assignChips config.ConfigChipDistribution
            |>> fun players ->
                {
                    Id = config.ConfigId
                    Name = config.ConfigName
                    Date = config.ConfigDate
                    Players = players
                    Games = []
                }
        else
            InvalidGamePlayersNumber numPlayers |> Error

    let id session = session.Id
    let players session = session.Players
    let games session = session.Games

    let name (GameSessionName name) = name

[<AutoOpen>]
module Patterns =

    let (|Chip|) (Chip chip) = chip
    
    let (|BetAmount|) (BetAmount amount) = amount
    
    let (|GameSessionName|) (GameSessionName name) = name

    let (|PlayerName|) (PlayerName name) = name
