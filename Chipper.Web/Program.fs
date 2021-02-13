module Chipper.Web.Program

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting

open Bolero.Server.RazorHost

let configureServices (services: IServiceCollection) =
    services.AddRazorPages().AddRazorRuntimeCompilation() |> ignore
    services.AddServerSideBlazor() |> ignore
    services.AddBoleroHost() |> ignore

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
