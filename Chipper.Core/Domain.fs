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
    | Blinds
    | Antes

type GameId = GameId of Guid

type Game = {
    Id : GameId
    Rounds : BettingRound list
}

type GameSessionId = GameSessionId of Guid

type GameSessionName = private GameSessionName of string

type GameSessionConfig = {
    ConfigId : GameSessionId
    ConfigName : GameSessionName
    ConfigDate : DateTime
    ConfigHost : Player
    ConfigPlayers : Player list
    ConfigRaiseType : RaiseType
    ConfigBettingType : BettingType
}

type GameSession = private {
    Id : GameSessionId
    Name : GameSessionName
    Date : DateTime
    Players : NonEmptyList<Player>
    RaiseType : RaiseType
    BettingType : BettingType
    Games : Game list
}

type JoiningPlayer = {
    GameSessionId : GameSessionId
    GameSessionName : GameSessionName
    Name : string
}

type ValidJoiningPlayer = {
    ValidGameSessionId : GameSessionId
    ValidGameSessionName : GameSessionName
    ValidName : PlayerName
}

type PlayerJoinInfo = {
    GameSessionId : GameSessionId
    PlayerName : PlayerName
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
module ValidJoiningPlayer =

    let create joinInfo (player : JoiningPlayer) =
        {
            ValidGameSessionId = player.GameSessionId
            ValidGameSessionName = player.GameSessionName
            ValidName = joinInfo.PlayerName
        }

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

    let defaultConfig id name date hostName =
        let host = { Name = hostName; Chips = [] }
        {
            ConfigId = id
            ConfigName = name
            ConfigDate = date
            ConfigHost = host
            ConfigPlayers = []
            ConfigRaiseType = NoLimit
            ConfigBettingType = Blinds
        }

    let fromConfig config =
        let numPlayers = config.ConfigPlayers |> List.length
        if numPlayers > 0 && numPlayers <= 20 then
            {
                Id = config.ConfigId
                Name = config.ConfigName
                Date = config.ConfigDate
                Players = NonEmptyList.create config.ConfigHost config.ConfigPlayers
                RaiseType = config.ConfigRaiseType
                BettingType = config.ConfigBettingType
                Games = []
            } |> Ok
        else
            InvalidGamePlayersNumber numPlayers |> Error

    let id session = session.Id
    let players session = session.Players
    let raiseType session = session.RaiseType
    let bettingType session = session.BettingType
    let games session = session.Games

    let name (GameSessionName name) = name

[<AutoOpen>]
module Patterns =

    let (|Chip|) (Chip chip) = chip
    
    let (|BetAmount|) (BetAmount amount) = amount
    
    let (|GameSessionName|) (GameSessionName name) = name

    let (|PlayerName|) (PlayerName name) = name
