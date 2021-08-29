namespace Chipper

type GetSessionError =
    | SessionNotFound of GameSessionId
    | GenericGetError of exn

type CreateSessionError = GenericCreateError of exn

type UpdateSessionError = GenericSaveError of exn

type DeleteSessionError = GenericDeleteError of exn

type PersistenceError =
    | GetSessionError of GetSessionError
    | CreateSessionError of CreateSessionError
    | SaveSessionError of UpdateSessionError
    | DeleteSessionError of DeleteSessionError
