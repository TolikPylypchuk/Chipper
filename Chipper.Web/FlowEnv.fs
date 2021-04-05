namespace Chipper.Web

open FSharpPlus
open FSharpPlus.Data

open Elmish

open Chipper.Core

type Env = {
    Storage : LocalStorage
    Repo : GameSessionRepository
    Mediator : IEventMediator
}

type Flow<'result> = ReaderT<Env, Writer<Cmd<Message> list, 'result>>

[<RequireQualifiedAccess>]
module Env =

    let askStorage : Flow<LocalStorage> = monad {
        let! env = ask
        return env.Storage
    }

    let askRepo : Flow<GameSessionRepository> = monad {
        let! env = ask
        return env.Repo
    }

    let askMediator : Flow<IEventMediator> = monad {
        let! env = ask
        return env.Mediator
    }
