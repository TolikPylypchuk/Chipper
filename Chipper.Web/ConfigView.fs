module Chipper.Web.ConfigView

open Bolero.Html

open Chipper.Core.Domain

let private configurePageHeader js state joinUrl =
    concat [
        h1 [ attr.class' "display-1 p-lg-4 p-md-3 p-2 text-center" ] [
            text $"Configure {state.Config.ConfigName |> GameSession.name}"
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
    ]

let private configurePagePlayer state (player : Player) name dispatch =
    concat [
        span [ attr.class' "me-3" ] [
            text name
        ]

        div [] [
            button [
                attr.type' "button"
                attr.class' "btn btn-secondary btn-sm m-1"
                on.click (fun _ -> dispatch <| Message.editPlayerName player.Name)
            ] [
                i [ attr.class' "bi bi-pencil-square" ] []
            ]

            button [
                attr.type' "button"
                attr.class' "btn btn-danger btn-sm m-1"
                attr.disabled (player.Name = state.Config.ConfigHost.Name)
                on.click (fun _ -> dispatch <| Message.removePlayer player.Name)
            ] [
                i [ attr.class' "bi bi-x-circle" ] []
            ]
        ]
    ]

let private configurePageEditedPlayer originalName editedName isPlayerNameValid dispatch =
    concat [
        input [
            attr.name "player-name"
            bind.input.string editedName (dispatch << Message.configInputPlayerName)
        ]

        div [] [
            button [
                attr.type' "button"
                attr.class' "btn btn-success btn-sm m-1"
                attr.disabled (not <| isPlayerNameValid originalName editedName)
                on.click (fun _ -> dispatch Message.acceptPlayerNameEdit)
            ] [
                i [ attr.class' "bi bi-check2-circle" ] []
            ]

            button [
                attr.type' "button"
                attr.class' "btn btn-danger btn-sm m-1"
                on.click (fun _ -> dispatch Message.cancelPlayerNameEdit)
            ] [
                i [ attr.class' "bi bi-x-circle" ] []
            ]
        ]
    ]

let private configurePagePlayers state isPlayerNameValid dispatch =
    section [ attr.class' "col-md-auto m-2 m-md-4" ] [
        h6 [] [
            text "Players"
        ]

        ul [ attr.class' "list-group w-100" ] [
            forEach (state.Config.ConfigHost :: state.Config.ConfigPlayers) <| fun player ->
                let (PlayerName name) = player.Name

                li [ attr.class' "list-group-item d-flex flex-row align-items-center justify-content-between" ] [
                    cond state.EditMode <| function
                        | Player (playerName', editedName) when playerName' = player.Name ->
                            configurePageEditedPlayer playerName' editedName isPlayerNameValid dispatch
                        | _ ->
                            configurePagePlayer state player name dispatch
                ]
        ]
    ]

let private configurePagePlayerRequests state dispatch =
    section [ attr.class' "col-md-auto m-2 m-md-4" ] [
        h6 [] [
            text "Player Requests"
        ]

        ul [ attr.class' "list-group w-100" ] [
            forEach state.PlayerRequests <| fun { PlayerName = ((PlayerName name) as playerName) } ->
                li [ attr.class' "list-group-item d-flex flex-row align-items-center justify-content-between" ] [
                    span [ attr.class' "me-3" ] [
                        text name
                    ]

                    div [] [
                        button [
                            attr.type' "button"
                            attr.class' "btn btn-success btn-sm m-1"
                            on.click (fun _ -> dispatch <| Message.acceptPlayerRequest playerName)
                        ] [
                            i [ attr.class' "bi bi-check2-circle" ] []
                        ]

                        button [
                            attr.type' "button"
                            attr.class' "btn btn-danger btn-sm m-1"
                            on.click (fun _ -> dispatch <| Message.rejectPlayerRequest playerName)
                        ] [
                            i [ attr.class' "bi bi-x-circle" ] []
                        ]
                    ]
                ]
        ]
    ]

let private configurePageBettingType state dispatch =
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
                    bind.change.string (string betType) (fun _ -> dispatch <| Message.setBettingType betType)
                ]

                label [ attr.for' inputId; attr.class' "form-check-label" ] [
                    text <| match betType with Blinds -> "Blinds" | Antes -> "Antes"
                ]
            ]
    ]

let private configurePageRaiseType state dispatch =
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
                    bind.change.string (string raiseType) (fun _ -> dispatch <| Message.setRaiseType raiseType)
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
    
let configurePage js state joinUrl isPlayerNameValid dispatch =
    div [ attr.class' "container" ] [
        configurePageHeader js state joinUrl
        
        div [ attr.class' "row justify-content-md-center" ] [
            configurePagePlayers state isPlayerNameValid dispatch
            configurePagePlayerRequests state dispatch
        ]

        div [ attr.class' "row justify-content-md-center" ] [
            configurePageBettingType state dispatch
            configurePageRaiseType state dispatch
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