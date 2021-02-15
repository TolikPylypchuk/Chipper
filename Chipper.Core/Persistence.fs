module Chipper.Core.Persistence

open Domain

type PersistentGameSession =
    | NewSession of GameSessionId
    | ConfigurableSession of GameSessionConfiguration
    | PersistentSession of GameSession

type GetSessionError =
    | SessionNotFound of GameSessionId
    | GenericGetError of exn

type SaveSessionError = GenericSaveError of exn

type DeleteSessionError = GenericDeleteError of exn

type PersistenceError =
    | GetSessionError of GetSessionError
    | SaveSessionError of SaveSessionError
    | DeleteSessionError of DeleteSessionError

type CreateSessionId = unit -> GameSessionId

type GetSession = GameSessionId -> Async<Result<PersistentGameSession, GetSessionError>>

type SaveSession = PersistentGameSession -> Async<Result<unit, SaveSessionError>>

type DeleteSession = GameSessionId -> Async<Result<unit, DeleteSessionError>>

type GameSessionRepository = {
    CreateId : CreateSessionId
    Get : GetSession
    Save : SaveSession
    Delete : DeleteSession
}

[<RequireQualifiedAccess>]
module PersistentGameSession =
    let id =
        function
        | NewSession id -> id
        | ConfigurableSession session -> session.Id
        | PersistentSession session -> session |> GameSession.id
