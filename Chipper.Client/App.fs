namespace Chipper.Client

open Elmish

open Bolero
open Bolero.Remoting.Client
open Bolero.Templating.Client

type Model = unit
type Message = unit

type MainTemplate = Template<"wwwroot/html/main.html">

module App =

    let update message model =
        (), Cmd.none

    let render model dispatch =
        MainTemplate().Elt()

type App() =
    inherit ProgramComponent<Model, Message>()

    override _.Program =
        Program.mkProgram (fun _ -> (), Cmd.none) App.update App.render
#if DEBUG
        |> Program.withHotReload
#endif
