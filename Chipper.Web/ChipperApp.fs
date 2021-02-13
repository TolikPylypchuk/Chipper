namespace Chipper.Web

open Elmish

open Bolero
open Bolero.Html

type Model =
    | StartModel
    | NotImplemented

type Message =
    | CreateSession

module ChipperApp =

    let update message model =
        NotImplemented, Cmd.none

    let render model dispatch =
        concat [
            h1 [ attr.``class`` "display-1 p-lg-4 p-md-3 p-2 text-center" ] [
                text "Chipper"
            ]

            p [ attr.``class`` "lead p-2 text-center" ] [
                text "Cards are on you. Chips are on me."
            ]

            div [ attr.``class`` "text-center p-2" ] [
                button [
                    attr.``type`` "button"
                    attr.``class`` "btn btn-primary btn-lg"
                    on.click (fun _ -> dispatch CreateSession)
                ] [
                    text "Create a session"
                ]
            ]

            if model = NotImplemented then
                p [ attr.``class`` "text-center text-danger" ] [
                    text "Not implemented yet"
                ]
        ]

type ChipperApp() =
    inherit ProgramComponent<Model, Message>()

    override _.Program =
        Program.mkProgram (fun _ -> StartModel, Cmd.none) ChipperApp.update ChipperApp.render
