module Chipper.Web.PlayView

open Bolero.Html

open Chipper

let awaitingGameStart (state : GameSessionState) dispatch =
    div [ attr.class' "h-100 d-flex align-items-center" ] [
        div [ attr.class' "container" ] [
            p [ attr.class' "lead m-2 text-center" ] [
                text "Click 'start' to become the first dealer"
            ]

            div [ attr.class' "text-center m-4" ] [
                button [
                    attr.type' "button"
                    attr.class' "btn btn-primary btn-lg"
                    on.click (fun _ -> dispatch Message.startGame)
                ] [
                    text "Start"
                ]
            ]
        ]
    ]
