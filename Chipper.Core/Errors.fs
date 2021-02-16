namespace Chipper.Core

type ChipperError =
    | ChipperDomainError of DomainError
    | ChipperPersistenceError of PersistenceError
