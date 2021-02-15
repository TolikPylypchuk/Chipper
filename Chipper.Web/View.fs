module Chipper.Web.View

open Bolero.Html

open Flurl

open Chipper.Core

let class' = attr.``class``
let type' = attr.``type``

let homePage dispatch =
    concat [
        h1 [ class' "display-1 m-lg-4 m-md-3 m-2 text-center" ] [
            text "Chipper"
        ]

        p [ class' "lead m-2 text-center" ] [
            text "Cards are on you. Chips are on me."
        ]

        div [ class' "text-center m-4" ] [
            button [
                type' "button"
                class' "btn btn-primary btn-lg"
                on.click (fun _ -> dispatch StartGameSession)
            ] [
                text "Start playing"
            ]
        ]
    ]

let startPage settings (GameSessionId sessionId) dispatch =
    concat [
        div [ class' "container" ] [
            h2 [ class' "display-2 m-lg-4 m-md-3 m-2 text-center" ] [
                text "Share the link"
            ]

            p [ class' "lead m-2 text-center" ] [
                text "And configure the game while others are joining"
            ]

            div [ class' "m-4 text-center" ] [
                code [ class' "fs-3" ] [
                    text <| Url.Combine(settings.UrlRoot, (router.Link <| JoinPage sessionId))
                ]
            ]

            div [ class' "text-center m-4" ] [
                button [
                    type' "button"
                    class' "btn btn-primary btn-lg"
                    on.click (fun _ -> dispatch ConfigureGameSession)
                ] [
                    text "Configure the game"
                ]
            ]
        ]
    ]

let notImplementedPage =
    div [ class' "position-relative vw-100 vh-100" ] [
        div [ class' "container position-absolute top-50 start-50 translate-middle" ] [
            h1 [ class' "display-1 text-center" ] [
                text "Hang tight, this one's not implemented yet"
            ]
        ]
    ]
