namespace Chipper.Web

open FSharpPlus
open FSharpPlus.Data

open Elmish

open Chipper.Core.Persistence

type Env = {
    Storage : LocalStorage
    Repo : GameSessionRepository
    Mediator : IEventMediator
}

[<RequireQualifiedAccess>]
module Env =

    let askStorage = Reader.ask |>> (fun env -> env.Storage)
    let askRepo = Reader.ask |>> (fun env -> env.Repo)
    let askMediator = Reader.ask |>> (fun env -> env.Mediator)

    let none = Reader.Return<_, _>
