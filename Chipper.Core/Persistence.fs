module Chipper.Core.Persistence

open Domain

type PersistentGameSession =
    | NewSession of NewGameSession
    | ConfigurableSession of GameSessionConfig
    | PersistentSession of GameSession

type GetSession = GameSessionId -> Async<Result<PersistentGameSession, PersistenceError>>

type CreateSession = GameSessionName -> PlayerName -> Async<Result<NewGameSession, PersistenceError>>

type UpdateSession = PersistentGameSession -> Async<Result<unit, PersistenceError>>

type DeleteSession = GameSessionId -> Async<Result<unit, PersistenceError>>

type GameSessionRepository = {
    GetSession : GetSession
    CreateSession : CreateSession
    UpdateSession : UpdateSession
    DeleteSession : DeleteSession
}

[<RequireQualifiedAccess>]
module PersistentGameSession =

    let id =
        function
        | NewSession session -> session.Id
        | ConfigurableSession session -> session.Id
        | PersistentSession session -> session |> GameSession.id
