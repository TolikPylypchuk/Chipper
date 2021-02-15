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

open Chipper.Core.Domain
open Chipper.Core.Persistence

let configureSettings (services: IServiceProvider) =
    let config = services.GetRequiredService<IConfiguration>()
    let appConfig = config.GetSection("App")
    let s = Unchecked.defaultof<AppSettings>
    { UrlRoot = appConfig.[nameof s.UrlRoot] }

let inMemoryRepository () =
    let storage = Dictionary<GameSessionId, PersistentGameSession>()

    {
        CreateId = Guid.NewGuid >> GameSessionId
        Get = fun id ->
            match storage.TryGetValue(id) with
            | true, session -> session |> Ok
            | _ -> id |> SessionNotFound |> Error
            |> async.Return
        Save = fun session -> storage.Add(session |> PersistentGameSession.id, session) |> Ok |> async.Return
        Delete = fun id -> storage.Remove(id) |> ignore |> Ok |> async.Return
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
    services.AddSingleton<AppSettings>(configureSettings) |> ignore
    services.AddSingleton<GameSessionRepository>(inMemoryRepository ()) |> ignore

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
