namespace Chipper.Web

open Elmish

open Bolero

type Model = unit
type Message = unit

type MainTemplate = Template<"wwwroot/html/main.html">

module ChipperApp =

    let update message model =
        (), Cmd.none

    let render model dispatch =
        MainTemplate().Elt()

type ChipperApp() =
    inherit ProgramComponent<Model, Message>()

    override _.Program =
        Program.mkProgram (fun _ -> (), Cmd.none) ChipperApp.update ChipperApp.render
