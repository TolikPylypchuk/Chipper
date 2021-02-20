[<AutoOpen>]
module Chipper.Core.ErrorWrappers

open FSharpPlus

open Domain
open Persistence

let asDomainError result = result |> Result.mapError DomainError

let asPersistenceError result = result |> Result.mapError PersistenceError

let chip = Chip.create >> Result.mapError ChipError >> asDomainError

let betAmount = BetAmount.create >> Result.mapError BetAmountError >> asDomainError

let playerName = PlayerName.create >> Result.mapError PlayerNameError >> asDomainError

let gameSessionName = GameSessionName.create >> Result.mapError GameSessionNameError >> asDomainError

let gameSession = GameSession.fromConfig >> Result.mapError GameSessionError >> asDomainError

let getSession id repo = repo.GetSession id |> Async.map asPersistenceError

let createSession name playerName repo = repo.CreateSession name playerName |> Async.map asPersistenceError

let updateSession session repo = repo.UpdateSession session |> Async.map asPersistenceError

let deleteSession id repo = repo.DeleteSession id |> Async.map asPersistenceError
