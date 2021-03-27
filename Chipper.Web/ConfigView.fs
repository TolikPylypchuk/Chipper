module Chipper.Web.ConfigView

open Bolero.Html

open Chipper.Core.Domain

let private configurePageHeader js state joinUrl isSessionNameValid dispatch =
    concat [
        h1 [ attr.class' "display-1 p-lg-4 p-md-3 p-2 text-center" ] [
            text $"Configure {state.Config.ConfigName |> GameSession.name}"
        ]

        cond state.EditMode <| function
            | EditSession name ->
                div [ attr.class' "d-flex flex-row align-items-center justify-content-center" ] [
                    input [
                        attr.name "session-name"
                        bind.input.string name (dispatch << Message.configInputSessionName)
                    ]

                    div [] [
                        button [
                            attr.type' "button"
                            attr.class' "btn btn-success btn-sm m-1"
                            attr.disabled (not <| isSessionNameValid name)
                            on.click (fun _ -> dispatch Message.acceptEdit)
                        ] [
                            i [ attr.class' "bi bi-check2-circle" ] []
                        ]

                        button [
                            attr.type' "button"
                            attr.class' "btn btn-danger btn-sm m-1"
                            on.click (fun _ -> dispatch Message.cancelEdit)
                        ] [
                            i [ attr.class' "bi bi-x-circle" ] []
                        ]
                    ]
                ]
            | _ ->
                empty

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

            button [
                attr.type' "button"
                attr.class' "btn btn-light btn-lg m-2"
                attr.disabled (state.EditMode <> NoEdit)
                on.click (fun _ -> dispatch Message.editSessionName)
            ] [
                text "Edit game name"
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
                attr.disabled (state.EditMode <> NoEdit)
                on.click (fun _ -> dispatch <| Message.editPlayerName player.Id)
            ] [
                i [ attr.class' "bi bi-pencil-square" ] []
            ]

            button [
                attr.type' "button"
                attr.class' "btn btn-danger btn-sm m-1"
                attr.disabled (player.Name = state.Config.ConfigHost.Name)
                on.click (fun _ -> dispatch <| Message.removePlayer player.Id)
            ] [
                i [ attr.class' "bi bi-x-circle" ] []
            ]
        ]
    ]

let private configurePageEditedPlayer playerId editedName isPlayerNameValid dispatch =
    concat [
        input [
            attr.name "player-name"
            bind.input.string editedName (dispatch << Message.configInputPlayerName)
        ]

        div [] [
            button [
                attr.type' "button"
                attr.class' "btn btn-success btn-sm m-1"
                attr.disabled (not <| isPlayerNameValid playerId editedName)
                on.click (fun _ -> dispatch Message.acceptEdit)
            ] [
                i [ attr.class' "bi bi-check2-circle" ] []
            ]

            button [
                attr.type' "button"
                attr.class' "btn btn-danger btn-sm m-1"
                on.click (fun _ -> dispatch Message.cancelEdit)
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
                        | EditPlayer (playerId, editedName) when playerId = player.Id ->
                            configurePageEditedPlayer playerId editedName isPlayerNameValid dispatch
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
            forEach state.Config.ConfigPlayerRequests <|
                fun { PlayerId = playerId; Info = { PlayerName = PlayerName name } } ->
                    li [ attr.class' "list-group-item d-flex flex-row align-items-center justify-content-between" ] [
                        span [ attr.class' "me-3" ] [
                            text name
                        ]

                        div [] [
                            button [
                                attr.type' "button"
                                attr.class' "btn btn-success btn-sm m-1"
                                on.click (fun _ -> dispatch <| Message.acceptPlayerRequest playerId)
                            ] [
                                i [ attr.class' "bi bi-check2-circle" ] []
                            ]

                            button [
                                attr.type' "button"
                                attr.class' "btn btn-danger btn-sm m-1"
                                on.click (fun _ -> dispatch <| Message.rejectPlayerRequest playerId)
                            ] [
                                i [ attr.class' "bi bi-x-circle" ] []
                            ]
                        ]
                    ]
        ]
    ]

let configurePage js state joinUrl isSessionNameValid isPlayerNameValid dispatch =
    div [ attr.class' "container" ] [
        configurePageHeader js state joinUrl isSessionNameValid dispatch
        
        div [ attr.class' "row justify-content-md-center" ] [
            configurePagePlayers state isPlayerNameValid dispatch
            configurePagePlayerRequests state dispatch
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
