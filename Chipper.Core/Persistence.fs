namespace Chipper

open FSharpPlus

[<RequireQualifiedAccess>]
module Persistence =
    
    let private asPersistenceError result = result |> Result.mapError PersistenceError

    let getSession id repo = repo.GetSession id |> Async.map asPersistenceError

    let createSession name playerName repo = repo.CreateSession name playerName |> Async.map asPersistenceError

    let updateSession session repo = repo.UpdateSession session |> Async.map asPersistenceError

    let deleteSession id repo = repo.DeleteSession id |> Async.map asPersistenceError
