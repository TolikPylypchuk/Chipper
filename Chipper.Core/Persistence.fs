module Chipper.Core.Persistence

open Domain

type GetSession = GameSessionId -> Async<Result<PersistentGameSession, GetSessionError>>

type CreateSession = GameSessionName -> Async<Result<NewGameSession, CreateSessionError>>

type UpdateSession = PersistentGameSession -> Async<Result<unit, UpdateSessionError>>

type DeleteSession = GameSessionId -> Async<Result<unit, DeleteSessionError>>

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
