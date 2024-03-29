namespace Chipper

type PersistentGameSession =
    | ConfigurableSession of GameSessionConfig
    | PersistentSession of GameSession

type GetSession = GameSessionId -> Async<Result<PersistentGameSession, PersistenceError>>

type CreateSession = GameSessionName -> PlayerName -> Async<Result<GameSessionConfig, PersistenceError>>

type UpdateSession = PersistentGameSession -> Async<Result<unit, PersistenceError>>

type DeleteSession = GameSessionId -> Async<Result<unit, PersistenceError>>

type GeneratePlayerId = unit -> Async<PlayerId>

type GenerateGameId = unit -> Async<GameId>

type GameSessionRepository = {
    GetSession : GetSession
    CreateSession : CreateSession
    UpdateSession : UpdateSession
    DeleteSession : DeleteSession
    GeneratePlayerId : GeneratePlayerId
    GenerateGameId : GenerateGameId
}

[<RequireQualifiedAccess>]
module PersistentGameSession =

    let id =
        function
        | ConfigurableSession session -> session.ConfigId
        | PersistentSession session -> session |> GameSession.id
