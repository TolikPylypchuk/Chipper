module Chipper.Web.View

open Bolero.Html

module attr =
    let class' = attr.``class``
    let type' = attr.``type``

    module aria =
        let describedby value = attr.aria "describedby" value

let homePage dispatch =
    concat [
        h1 [ attr.class' "display-1 m-lg-4 m-md-3 m-2 text-center" ] [
            text "Chipper"
        ]

        p [ attr.class' "lead m-2 text-center" ] [
            text "Cards are on you. Chips are on me."
        ]

        div [ attr.class' "text-center m-4" ] [
            button [
                attr.type' "button"
                attr.class' "btn btn-primary btn-lg"
                on.click (fun _ -> dispatch StartGameSession)
            ] [
                text "Start playing"
            ]
        ]
    ]

let startPage isValid dispatch =
    concat [
        div [ attr.class' "container" ] [
            h2 [ attr.class' "display-4 m-lg-4 m-md-3 m-2 text-center" ] [
                text "Describe Your Poker Game"
            ]

            div [ attr.class' "m-4" ] [
                input [
                    attr.type' "text"
                    attr.class' "form-control"
                    attr.aria.describedby "session-name-help"
                    on.input (fun e -> dispatch (e.Value.ToString() |> DebounceStart |> InputSessionName))
                ]

                div [ attr.id "session-name-help"; attr.class' "form-text" ] [
                    text "e.g. Dave's Poker Game"
                ]
            ]

            div [ attr.class' "text-center m-4" ] [
                button [
                    attr.type' "button"
                    attr.class' "btn btn-primary btn-lg"
                    if not isValid then attr.disabled ""
                    on.click (fun _ -> dispatch SaveSessionName)
                ] [
                    text "Invite Friends"
                ]
            ]
        ]
    ]

let invitePage joinUrl dispatch =
    concat [
        div [ attr.class' "container" ] [
            h2 [ attr.class' "display-4 m-lg-4 m-md-3 m-2 text-center" ] [
                text "Share the link"
            ]

            p [ attr.class' "lead m-2 text-center" ] [
                text "And configure the game while others are joining"
            ]

            div [ attr.class' "m-4 text-center" ] [
                code [ attr.class' "fs-3" ] [
                    text joinUrl
                ]
            ]

            div [ attr.class' "text-center m-4" ] [
                button [
                    attr.type' "button"
                    attr.class' "btn btn-primary btn-lg"
                    on.click (fun _ -> dispatch ConfigureGameSession)
                ] [
                    text "Configure the game"
                ]
            ]
        ]
    ]

let notImplementedPage =
    div [ attr.class' "position-relative vw-100 vh-100" ] [
        div [ attr.class' "container position-absolute top-50 start-50 translate-middle" ] [
            h1 [ attr.class' "display-1 text-center" ] [
                text "Hang tight, this one's not implemented yet"
            ]
        ]
    ]
