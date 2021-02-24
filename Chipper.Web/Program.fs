module Chipper.Web.Program

open System
open System.Collections.Generic
open System.Text.Json.Serialization

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting

open Blazored.LocalStorage
open Bolero.Server.RazorHost

open Chipper.Core
open Chipper.Core.Domain
open Chipper.Core.Persistence

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

let configureServices (services: IServiceCollection) =
    services.AddRazorPages()
        .AddJsonOptions(fun options -> options.JsonSerializerOptions.Converters.Add(JsonFSharpConverter()))
        .AddRazorRuntimeCompilation()
        |> ignore

    services.AddBlazoredLocalStorage(fun options ->
        options.JsonSerializerOptions.Converters.Add(JsonFSharpConverter()))
        |> ignore

    services.AddServerSideBlazor() |> ignore
    services.AddBoleroHost(server = true) |> ignore
    services
        .AddSingleton<AppSettings>(settings)
        .AddSingleton<GameSessionRepository>(inMemoryRepository ())
        .AddScoped<LocalStorage>(fun services -> localStorage services)
        |> ignore

let configure (ctx : WebHostBuilderContext) (app: IApplicationBuilder) =
    if not <| ctx.HostingEnvironment.IsProduction() then
        app.UseDeveloperExceptionPage() |> ignore

    app.UseStaticFiles()
        .UseRouting()
        .UseEndpoints(fun endpoints ->
            endpoints.MapBlazorHub() |> ignore
            endpoints.MapFallbackToPage("/_Host") |> ignore)
        |> ignore

let createHostBuilder args =
    Host.CreateDefaultBuilder(args)
        .UseSystemd()
        .ConfigureWebHostDefaults(fun webBuilder ->
            webBuilder
                .ConfigureServices(configureServices)
                .Configure(configure)
            |> ignore)

[<EntryPoint>]
let main args =
    createHostBuilder(args).Build().Run()
    0
