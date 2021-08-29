module Chipper.Web.GameStartView

open Bolero.Html

open Chipper

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
                    on.click (fun _ -> dispatch Message.startGameSessionConfig)
                ] [
                    text "Start playing"
                ]
            ]
        ]
    ]

let startPage isReadonly (state : AddSessionState) dispatch =
    div [ attr.class' "h-100 d-flex align-items-center" ] [
        div [ attr.class' "container" ] [
            h2 [ attr.class' "display-4 m-lg-4 m-md-3 m-2 text-center" ] [
                text "Describe Your Poker Game"
            ]
                
            div [ attr.class' "m-4" ] [
                input [
                    attr.type' "text"
                    attr.class' "form-control"
                    attr.readonly <| isReadonly
                    attr.placeholder "Your Name"
                    bind.input.string state.HostName (Message.inputPlayerName >> dispatch)
                ]
            ]

            div [ attr.class' "m-4" ] [
                input [
                    attr.type' "text"
                    attr.class' "form-control"
                    attr.readonly <| isReadonly
                    attr.placeholder "Game Name"
                    attr.aria "describedby" "session-name-help"
                    bind.input.string state.SessionName (Message.inputSessionName >> dispatch)
                ]

                div [ attr.id "session-name-help"; attr.class' "form-text" ] [
                    text "e.g. Dave's Poker Game"
                ]
            ]

            div [ attr.class' "text-center m-4" ] [
                cond state.Target <| function
                    | Ok target ->
                        button [
                            attr.type' "button"
                            attr.class' "btn btn-primary btn-lg"
                            on.click (fun _ -> dispatch <| Message.saveSessionName target)
                        ] [
                            text "Configure the game"
                        ]
                    | _ ->
                        button [
                            attr.type' "button"
                            attr.class' "btn btn-primary btn-lg"
                            attr.disabled true
                        ] [
                            text "Configure the game"
                        ]
            ]
        ]
    ]

let joinPage { Name = name; GameSessionName = GameSessionName sessionName; Target = newPlayer } dispatch =
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
                    bind.input.string name (Message.inputPlayerName >> dispatch)
                ]
            ]

            div [ attr.class' "text-center m-4" ] [
                cond newPlayer <| function
                    | Ok player ->
                        button [
                            attr.type' "button"
                            attr.class' "btn btn-primary btn-lg"
                            on.click (fun _ -> dispatch <| Message.requestAccess player)
                        ] [
                            text "Request access"
                        ]
                    | _ ->
                        button [
                            attr.type' "button"
                            attr.class' "btn btn-primary btn-lg"
                            attr.disabled true
                        ] [
                            text "Request access"
                        ]
            ]
        ]
    ]

let awaitJoinPage (GameSessionName sessionName) dispatch =
    div [ attr.class' "h-100 d-flex align-items-center" ] [
        div [ attr.class' "container" ] [
            h2 [ attr.class' "display-4 m-lg-4 m-md-3 m-2 text-center" ] [
                text <| "Join " + sessionName
            ]

            div [ attr.class' "text-center" ] [
                div [ attr.class' "progress m-4" ] [
                    div [
                        attr.class' "progress-bar progress-bar-striped progress-bar-animated w-100"
                        attr.role "progressbar"
                        attr.aria "valuenow" 50
                        attr.aria "valuemin" 0
                        attr.aria "valuemax" 100
                    ] []
                ]

                p [] [
                    text "Awaiting confirmation . . ."
                ]

                div [ attr.class' "text-center m-4" ] [
                    button [
                        attr.type' "button"
                        attr.class' "btn btn-primary btn-lg"
                        on.click (fun _ -> dispatch Message.cancelRequest)
                    ] [
                        text "Cancel request"
                    ]
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

let lobbyPage (GameSessionName sessionName) renameInfo dispatch =
    concat [
        div [ attr.class' "h-100 d-flex align-items-center" ] [
            div [ attr.class' "container" ] [
                h2 [ attr.class' "display-4 m-lg-4 m-md-3 m-2 text-center" ] [
                    text <| "Join " + sessionName
                ]

                p [ attr.class' "lead text-center" ] [
                    text "You were accepted!"
                ]

                div [ attr.class' "text-center" ] [
                    div [ attr.class' "progress m-4" ] [
                        div [
                            attr.class' "progress-bar progress-bar-striped progress-bar-animated w-100"
                            attr.role "progressbar"
                            attr.aria "valuenow" 50
                            attr.aria "valuemin" 0
                            attr.aria "valuemax" 100
                        ] []
                    ]

                    p [] [
                        text "Now waiting for the game to start . . ."
                    ]
                    
                    div [ attr.class' "text-center m-4" ] [
                        button [
                            attr.type' "button"
                            attr.class' "btn btn-primary btn-lg"
                            on.click (fun _ -> dispatch Message.cancelRequest)
                        ] [
                            text "Cancel request"
                        ]
                    ]
                ]
            ]
        ]

        cond renameInfo <| function
            | Some renameInfo ->
                ToastComponent.playerRenamedNotification (renameInfo.HostName, renameInfo.NewName) dispatch
            | None ->
                empty
    ]

let rejectedJoinPage (GameSessionName sessionName) newPlayer wasAlreadyAdded dispatch =    
    div [ attr.class' "h-100 d-flex align-items-center" ] [
        div [ attr.class' "container" ] [
            h2 [ attr.class' "display-4 m-lg-4 m-md-3 m-2 text-center" ] [
                text <| "Join " + sessionName
            ]

            p [ attr.class' "lead text-center" ] [
                cond wasAlreadyAdded <| function
                    | true -> text "You were removed from the game!"
                    | false -> text "You were rejected!"
            ]

            div [ attr.class' "text-center m-4" ] [
                button [
                    attr.type' "button"
                    attr.class' "btn btn-primary btn-lg"
                    on.click (fun _ -> dispatch <| Message.requestAccessAgain newPlayer)
                ] [
                    text "Request access again"
                ]
            ]
        ]
    ]

let joinRequestCanceledPage (GameSessionName sessionName) =
    div [ attr.class' "h-100 d-flex align-items-center" ] [
        div [ attr.class' "container" ] [
            h2 [ attr.class' "display-4 m-lg-4 m-md-3 m-2 text-center" ] [
                text <| "Join " + sessionName
            ]

            h3 [ attr.class' "display-5 m-2 text-center" ] [
                text "Request canceled"
            ]

            p [ attr.class' "lead m-2 text-center" ] [
                text "Reload the page to request again"
            ]
        ]
    ]
