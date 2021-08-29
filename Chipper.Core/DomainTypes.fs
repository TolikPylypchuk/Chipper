namespace Chipper

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

type ConfigPlayerList = private {
    Host : Player
    Players : List<Player>
}

type BettingType = NoBetsBeforeStart

type RaiseType = NoLimit

type BetRoundNumber = private BetRoundNumber of int

type GameSessionConfig = {
    ConfigId : GameSessionId
    ConfigName : GameSessionName
    ConfigPlayerRequests : PlayerJoinRequest list
    ConfigDate : DateTime
    ConfigPlayers : ConfigPlayerList
    ConfigChipDistribution : ChipDistribution
    ConfigBetRoundNumber : int
    ConfigBettingType : BettingType
    ConfigRaiseType : RaiseType
}

type PlayerList = private PlayerList of NonEmptyList<Player>

type GameSession = private {
    Id : GameSessionId
    Name : GameSessionName
    Date : DateTime
    Players : PlayerList
    BetRoundNumber : BetRoundNumber
    BettingType : BettingType
    RaiseType : RaiseType
    Games : Game list
}

[<RequireQualifiedAccess>]
module Chip =

    let minValue = 0
    let maxValue = 1_000_000

    let create value =
        if value > minValue && value <= maxValue
        then Chip value |> Ok
        else ChipValueOutOfRange value |> Error

    let value (Chip chip) = chip

    let one () = Chip 1
    let five () = Chip 5
    let ten () = Chip 10
    let twentyFive () = Chip 25
    let oneHundred () = Chip 100

[<RequireQualifiedAccess>]
module BetAmount =

    let minValue = 0
    let maxValue = 2_000_000_000

    let create amount =
        if amount > minValue && amount <= maxValue
        then BetAmount amount |> Ok
        else BetAmoutOutOfRange amount |> Error

    let value (BetAmount amount) = amount

[<RequireQualifiedAccess>]
module PlayerName =

    let maxLength = 50

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
    
    let id (player : Player) = player.Id

[<AutoOpen>]
module PlayerList =

    let initConfig host = { Host = host; Players = [] }

    let configHost players = players.Host

    let configPlayers (players : ConfigPlayerList) = players.Players

    let configValue players = NonEmptyList.create players.Host players.Players
    
    let value (PlayerList players) = players

    let fromConfig players = PlayerList <| NonEmptyList.create players.Host players.Players
    
    let private isUnique (players : NonEmptyList<Player>) playerName =
        players |> NonEmptyList.map (fun player -> player.Name <> playerName) |> NonEmptyList.reduce (&&)
    
    let private apendNumberIfNotUnique players playerName =
        let isUnique = isUnique players
        if playerName |> isUnique then
            playerName
        else
            2
            |> Seq.unfold (fun num -> Some (playerName |> PlayerName.appendNumber num, num + 1))
            |> Seq.filter isUnique
            |> Seq.head

    let addPlayer playerId playerName players =
        let actualName = playerName |> apendNumberIfNotUnique (players |> configValue)
        let newPlayer = { Id = playerId; Name = actualName; Chips = [] }
        { players with Players = players.Players @ [ newPlayer ] }, actualName

    let removePlayer playerId (players : ConfigPlayerList) =
        { players with Players = players.Players |> List.filter (fun player -> player.Id <> playerId) }
        
    let private isPlayerNameValid players playerId name =
        players
        |> configValue
        |> NonEmptyList.map (fun player -> player.Id = playerId || player.Name <> name)
        |> NonEmptyList.reduce (&&)

    let updatePlayer (playerId : PlayerId) (editedName : PlayerName) (player : Player) =
        if player.Id = playerId
        then { player with Name = editedName }
        else player
        
    let editPlayerName playerId (players : ConfigPlayerList) editedName =
        if editedName |> isPlayerNameValid players playerId then
            if playerId = players.Host.Id then
                { players with Host = { players.Host with Name = editedName } }
            else
                { players with Players = players.Players |> List.map (updatePlayer playerId editedName) }
            |> Ok
        else
            editedName |> PlayerName.value |> DuplicatePlayerName |> Error

    let movePlayer shouldSwap (players : ConfigPlayerList) =
        let rec swap (players : Player list) =
            match players with
            | first :: second :: rest when shouldSwap (first, second) -> second :: first :: rest
            | head :: tail -> head :: (swap tail)
            | players -> players

        { players with Players = players.Players |> swap }

    let movePlayerUp playerId = movePlayer (snd >> Player.id >> (=) playerId)

    let movePlayerDown playerId = movePlayer (fst >> Player.id >> (=) playerId)

[<RequireQualifiedAccess>]
module GameSessionName =

    let maxLength = 50

    let create name =
        if String.IsNullOrWhiteSpace(name) then
            EmptyGameSessionName |> Error
        else
            let name = name.Trim()
            let nameLength = name |> String.length
            if nameLength <= maxLength
            then GameSessionName name |> Ok
            else GameSessionNameTooLong name |> Error

    let value (GameSessionName name) = name

[<RequireQualifiedAccess>]
module BetRoundNumber =

    let min = 1
    let max = 20

    let defaultNumber = BetRoundNumber 4

    let create num =
        if num >= min && num <= max
        then num |> BetRoundNumber |> Ok
        else num |> BetRoundNumberOutOfRange |> Error

    let value (BetRoundNumber num) = num
    
[<RequireQualifiedAccess>]
module GameSession =

    let minPlayers = 2
    let maxPlayers = 20

    let defaultConfig id name date hostId hostName =
        {
            ConfigId = id
            ConfigName = name
            ConfigDate = date
            ConfigPlayers = PlayerList.initConfig { Id = hostId; Name = hostName; Chips = [] }
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
            ConfigBetRoundNumber = BetRoundNumber.defaultNumber |> BetRoundNumber.value
            ConfigBettingType = NoBetsBeforeStart
            ConfigRaiseType = NoLimit
        }

    let private assignChips (EqualChipDitribution chips) (PlayerList players) =
        let chipNumbers = chips |> Map.toList |> List.map snd
        if chipNumbers |> List.exists ((>) 0) || chipNumbers |> List.forall ((=) 0) then
            Error InvalidChipDistribution
        else
            let chipsPerPlayer =
                chips
                |> Map.toList
                |> List.collect (fun (chip, amount) -> chip |> List.replicate amount)

            players
            |> NonEmptyList.map (fun player -> { player with Chips = chipsPerPlayer })
            |> PlayerList
            |> Ok

    let fromConfig config =
        let numPlayers = 1 + (config.ConfigPlayers.Players |> List.length)
        if numPlayers >= minPlayers && numPlayers <= maxPlayers then
            let allPlayers = PlayerList.fromConfig config.ConfigPlayers
            monad {
                let! players = allPlayers |> assignChips config.ConfigChipDistribution
                let! betRoundNumber =
                    config.ConfigBetRoundNumber
                    |> BetRoundNumber.create
                    |> Result.mapError InvalidBetRoundNumber

                return {
                    Id = config.ConfigId
                    Name = config.ConfigName
                    Date = config.ConfigDate
                    Players = players
                    BetRoundNumber = betRoundNumber
                    BettingType = config.ConfigBettingType
                    RaiseType = config.ConfigRaiseType
                    Games = []
                }
            }
        else
            GamePlayersNumberOutOfRange numPlayers |> Error

    let id session = session.Id
    let name session = session.Name
    let date session = session.Date
    let players session = session.Players
    let betRoundNumber session = session.BetRoundNumber
    let bettingType session = session.BettingType
    let raiseType session = session.RaiseType
    let games session = session.Games

[<AutoOpen>]
module Patterns =

    let (|Chip|) (Chip chip) = chip
    
    let (|BetAmount|) (BetAmount amount) = amount
    
    let (|GameSessionName|) (GameSessionName name) = name

    let (|PlayerName|) (PlayerName name) = name
