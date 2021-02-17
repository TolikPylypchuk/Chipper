module Chipper.Web.View

open Bolero.Html

module attr =
    let class' = attr.``class``
    let type' = attr.``type``

    module aria =
        let describedby value = attr.aria "describedby" value

let homePage dispatch =
    div [ attr.class' "h-100 d-flex align-items-center" ] [
        div [ attr.class' "container" ] [
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
    ]

let startPage isValid dispatch =
    div [ attr.class' "h-100 d-flex align-items-center" ] [
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

let invitePage js joinUrl dispatch =
    div [ attr.class' "h-100 d-flex align-items-center" ] [
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

            div [ attr.class' "d-flex flex-row justify-content-center m-2" ] [
                button [
                    attr.type' "button"
                    attr.class' "btn btn-light btn-lg m-2"
                    on.task.click (fun _ -> js |> writeTextToClipboard joinUrl)
                ] [
                    text "Copy the link"
                ]

                button [
                    attr.type' "button"
                    attr.class' "btn btn-primary btn-lg m-2"
                    on.click (fun _ -> dispatch ConfigureGameSession)
                ] [
                    text "Configure the game"
                ]
            ]
        ]
    ]
    
let joinPage sessionName newPlayer dispatch =
    let isValid = match newPlayer with Ok _ -> true | _ -> false

    div [ attr.class' "h-100 d-flex align-items-center" ] [
        div [ attr.class' "container" ] [
            h2 [ attr.class' "display-4 m-lg-4 m-md-3 m-2 text-center" ] [
                text <| "Join " + sessionName
            ]

            div [ attr.class' "m-4" ] [
                input [
                    attr.type' "text"
                    attr.class' "form-control"
                    attr.placeholder "Your Name"
                    on.input (fun e -> dispatch (e.Value.ToString() |> DebounceStart |> InputPlayerName))
                ]
            ]

            div [ attr.class' "text-center m-4" ] [
                button [
                    attr.type' "button"
                    attr.class' "btn btn-primary btn-lg"
                    if not isValid then attr.disabled ""
                    on.click (fun _ -> match newPlayer with Ok player -> dispatch <| RequestAccess player | _ -> ())
                ] [
                    text "Request Access"
                ]
            ]
        ]
    ]

let invalidJoinPage =
    div [ attr.class' "h-100 d-flex align-items-center justify-content-center" ] [
        h1 [ attr.class' "display-1" ] [
            text "The game you're trying to join wasn't found :("
        ]
    ]

let notImplementedPage =
    div [ attr.class' "h-100 d-flex align-items-center justify-content-center" ] [
        h1 [ attr.class' "display-1" ] [
            text "Hang tight, this one's not implemented yet"
        ]
    ]
