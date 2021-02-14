module Chipper.Web.Program

open System

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting

open Bolero.Server.RazorHost

let configureSettings (services: IServiceProvider) =
    let config = services.GetRequiredService<IConfiguration>()
    let appConfig = config.GetSection("App")
    let s = Unchecked.defaultof<AppSettings>
    { UrlRoot = appConfig.[nameof s.UrlRoot] }

let configureServices (services: IServiceCollection) =
    services.AddRazorPages().AddRazorRuntimeCompilation() |> ignore
    services.AddServerSideBlazor() |> ignore
    services.AddBoleroHost(server = true) |> ignore
    services.AddSingleton<AppSettings>(configureSettings) |> ignore

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
