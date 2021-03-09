module Chipper.Web.Services

open System
open System.Collections.Generic

open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Configuration

open FSharp.Control.Reactive

open Blazored.LocalStorage

open Chipper.Core
open Chipper.Core.Domain
open Chipper.Core.Persistence
open Chipper.Web

type EventSubject = EventSubject of System.Reactive.Subjects.Subject<GameSessionEvent>

type RxEventMediator (subject : EventSubject) =

    let (EventSubject events) = subject
    let mutable subscription = Unchecked.defaultof<IDisposable>

    interface IEventMediator with

        member _.Post event =
                events
                |> Subject.onNext event
                |> ignore
                
        member _.Subscribe id callback =
            subscription <-
                events
                |> Observable.filter (fun event -> event.GameSessionId = id)
                |> Observable.map (fun event -> event.Event)
                |> Observable.subscribe callback

    interface IDisposable with

        member _.Dispose() =
            if subscription <> null then
                subscription.Dispose()
                subscription <- null

let settings (services: IServiceProvider) =
    let config = services.GetRequiredService<IConfiguration>()
    let appConfig = config.GetSection("App")
    let s = Unchecked.defaultof<AppSettings>
    { UrlRoot = appConfig.[nameof s.UrlRoot] }

let localStorage (services : IServiceProvider) =
    let localStorage = services.GetRequiredService<ILocalStorageService>()
    
    {
        GetState = fun () -> LocalStorage.getLocalState localStorage
        SetState = LocalStorage.setLocalState localStorage
        ClearState = fun () -> LocalStorage.clearLocalState localStorage
    }

let inMemoryRepository () =
    let storage = Dictionary<GameSessionId, PersistentGameSession>()

    {
        GetSession = fun id -> async {
            return
                match storage.TryGetValue(id) with
                | true, session -> session |> Ok
                | _ -> id |> SessionNotFound |> GetSessionError |> Error
        }

        CreateSession = fun name hostName -> async {
            let id = Guid.NewGuid() |> GameSessionId
            let config = GameSession.defaultConfig id name DateTime.Now hostName
            storage.Add(id, ConfigurableSession config)
            return Ok config
        }

        UpdateSession = fun session -> async { return (storage.[session |> PersistentGameSession.id] <- session) |> Ok }

        DeleteSession = fun id -> async { return storage.Remove(id) |> ignore |> Ok }
    }