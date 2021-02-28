module Chipper.Web.Program

open System.Text.Json.Serialization

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting

open Blazored.LocalStorage
open Bolero.Server.RazorHost

open Chipper.Web.Services

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
        .AddSingleton(inMemoryRepository ())
        .AddSingleton(rxEventMediator ())
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
