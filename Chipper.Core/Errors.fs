namespace Chipper.Core

type ChipperError =
    | DomainError of DomainError
    | PersistenceError of PersistenceError
