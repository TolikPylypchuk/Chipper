module Chipper.Web.View

open Bolero.Html

open Chipper.Core.Domain

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

let startPage isValid isReadonly sessionName playerName dispatch =
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
                    bind.input.string playerName (InputPlayerName >> dispatch)
                ]
            ]

            div [ attr.class' "m-4" ] [
                input [
                    attr.type' "text"
                    attr.class' "form-control"
                    attr.readonly <| isReadonly
                    attr.placeholder "Game Name"
                    attr.aria "describedby" "session-name-help"
                    bind.input.string sessionName (InputSessionName >> dispatch)
                ]

                div [ attr.id "session-name-help"; attr.class' "form-text" ] [
                    text "e.g. Dave's Poker Game"
                ]
            ]

            div [ attr.class' "text-center m-4" ] [
                button [
                    attr.type' "button"
                    attr.class' "btn btn-primary btn-lg"
                    attr.disabled <| not isValid
                    on.click (fun _ -> dispatch SaveSessionName)
                ] [
                    text "Configure the game"
                ]
            ]
        ]
    ]

let joinPage sessionName (newPlayer : Result<PlayerJoinInfo, _>) isAwaiting dispatch =
    let name, isValid = match newPlayer with Ok player -> player.PlayerName |> PlayerName.value, true | _ -> "", false

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
                    bind.input.string name (InputPlayerName >> dispatch)
                ]
            ]

            cond isAwaiting <| function
                | false ->
                    div [ attr.class' "text-center m-4" ] [
                        button [
                            attr.type' "button"
                            attr.class' "btn btn-primary btn-lg"
                            attr.disabled (not isValid || isAwaiting)
                            on.click (fun _ ->
                                match newPlayer with
                                | Ok player -> dispatch <| RequestAccess player
                                | _ -> ())
                        ] [
                            text "Request Access"
                        ]
                    ]
                | true ->
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
                    ]
        ]
    ]

let invalidJoinPage =
    div [ attr.class' "h-100 d-flex align-items-center justify-content-center" ] [
        h1 [ attr.class' "display-1" ] [
            text "The game you're trying to join wasn't found :("
        ]
    ]
    
let configurePage js state joinUrl isPlayerNameValid dispatch =
    div [ attr.class' "container" ] [
        h1 [ attr.class' "display-1 p-lg-4 p-md-3 p-2 text-center" ] [
            text $"Configure {state.Config.ConfigName}"
        ]

        p [ attr.class' "lead m-2 text-center" ] [
            text "And share the link for others to join"
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
        ]
        
        div [ attr.class' "row justify-content-md-center" ] [
            section [ attr.class' "col-md-auto m-2 m-md-4" ] [
                h6 [] [
                    text "Players"
                ]

                ul [ attr.class' "list-group w-100" ] [
                    forEach (state.Config.ConfigHost :: state.Config.ConfigPlayers) <| fun player ->
                        let (PlayerName name) = player.Name

                        li [
                            attr.class' "list-group-item d-flex flex-row align-items-center justify-content-between"
                        ] [
                            cond state.EditMode <| function
                                | Player (playerName', editedName) when playerName' = player.Name ->
                                    concat [
                                        input [
                                            attr.name "player-name"
                                            bind.input.string editedName (dispatch << InputPlayerName)
                                        ]

                                        div [] [
                                            button [
                                                attr.type' "button"
                                                attr.class' "btn btn-success btn-sm m-1"
                                                attr.disabled (not <| isPlayerNameValid editedName)
                                                on.click (fun _ -> dispatch AcceptEdit)
                                            ] [
                                                i [ attr.class' "bi bi-check2-circle" ] []
                                            ]

                                            button [
                                                attr.type' "button"
                                                attr.class' "btn btn-danger btn-sm m-1"
                                                on.click (fun _ -> dispatch CancelEdit)
                                            ] [
                                                i [ attr.class' "bi bi-x-circle" ] []
                                            ]
                                        ]
                                    ]
                                | _ ->
                                    concat [
                                        span [ attr.class' "me-3" ] [
                                            text name
                                        ]

                                        div [] [
                                            button [
                                                attr.type' "button"
                                                attr.class' "btn btn-secondary btn-sm m-1"
                                                on.click (fun _ -> dispatch <| EditPlayerName player.Name)
                                            ] [
                                                i [ attr.class' "bi bi-pencil-square" ] []
                                            ]

                                            button [
                                                attr.type' "button"
                                                attr.class' "btn btn-danger btn-sm m-1"
                                                attr.disabled (player.Name = state.Config.ConfigHost.Name)
                                            ] [
                                                i [ attr.class' "bi bi-x-circle" ] []
                                            ]
                                        ]
                                    ]
                        ]
                ]
            ]

            section [ attr.class' "col-md-auto m-2 m-md-4" ] [
                h6 [] [
                    text "Player Requests"
                ]

                ul [ attr.class' "list-group w-100" ] [
                    forEach state.PlayerRequests <| fun { PlayerName = (PlayerName name) } ->
                        li [
                            attr.class' "list-group-item d-flex flex-row align-items-center justify-content-between"
                        ] [
                            span [ attr.class' "me-3" ] [
                                text name
                            ]

                            div [] [
                                button [
                                    attr.type' "button"
                                    attr.class' "btn btn-success btn-sm m-1"
                                ] [
                                    i [ attr.class' "bi bi-check2-circle" ] []
                                ]

                                button [
                                    attr.type' "button"
                                    attr.class' "btn btn-danger btn-sm m-1"
                                ] [
                                    i [ attr.class' "bi bi-x-circle" ] []
                                ]
                            ]
                        ]
                ]
            ]
        ]

        div [ attr.class' "row justify-content-md-center" ] [
            section [ attr.class' "col-md-auto m-2 m-md-4" ] [
                h6 [] [
                    text <| "Betting Type"
                ]

                forEach [ Blinds; Antes ] <| fun betType ->
                    let inputId = sprintf "bet-%O" betType
                    div [ attr.class' "form-check" ] [
                        input [
                            attr.id <| inputId
                            attr.name "bet"
                            attr.type' "radio"
                            attr.class' "form-check-input"
                            attr.checked' (state.Config.ConfigBettingType = betType)
                            bind.change.string (string betType) (fun _ -> dispatch <| SetBettingType betType)
                        ]

                        label [ attr.for' inputId; attr.class' "form-check-label" ] [
                            text <| match betType with Blinds -> "Blinds" | Antes -> "Antes"
                        ]
                    ]
            ]

            section [ attr.class' "col-md-auto m-2 m-md-4" ] [
                h6 [] [
                    text <| "Raise Type"
                ]

                forEach [ NoLimit; Limit; PotLimit ] <| fun raiseType ->
                    let inputId = sprintf "raise-%O" raiseType

                    div [ attr.class' "form-check" ] [
                        input [
                            attr.id <| inputId
                            attr.name "raise"
                            attr.type' "radio"
                            attr.class' "form-check-input"
                            attr.checked' (state.Config.ConfigRaiseType = raiseType)
                            bind.change.string (string raiseType) (fun _ -> dispatch <| SetRaiseType raiseType)
                        ]

                        label [ attr.for' inputId; attr.class' "form-check-label" ] [
                            match raiseType with
                            | Limit -> "Limited"
                            | NoLimit -> "Unlimited"
                            | PotLimit -> "Pot-limited"
                            |> text
                        ]
                    ]
            ]
        ]

        div [ attr.class' "d-flex flex-row justify-content-center m-2" ] [
            button [
                attr.type' "button"
                attr.class' "btn btn-primary btn-lg m-2"
                attr.disabled true
            ] [
                text "Start the game"
            ]
        ]
    ]
    
let notImplementedPage =
    div [ attr.class' "h-100 d-flex align-items-center justify-content-center" ] [
        h1 [ attr.class' "display-1 text-center" ] [
            text "Hang tight, this one's not implemented yet"
        ]
    ]
