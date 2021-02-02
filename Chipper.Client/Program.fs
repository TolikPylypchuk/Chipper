module Chipper.Client.Program

open Microsoft.AspNetCore.Components.WebAssembly.Hosting

open Bolero.Remoting.Client

[<EntryPoint>]
let main args =
    let builder = WebAssemblyHostBuilder.CreateDefault(args)
    builder.RootComponents.Add<App>("#main")
    builder.Services.AddRemoting(builder.HostEnvironment) |> ignore
    builder.Build().RunAsync() |> ignore
    0
