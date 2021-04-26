module Chipper.Web.ConfigView

open Bolero.Html

open Chipper.Core

let private header js state joinUrl dispatch =
    concat [
        h1 [ attr.class' "display-1 p-lg-4 p-md-3 p-2 text-center" ] [
            text $"Configure {state.Config.ConfigName |> GameSession.name}"
        ]

        cond state.EditMode <| function
            | EditSession { Name = name; Target = target } ->
                div [ attr.class' "d-flex flex-row align-items-center justify-content-center" ] [
                    input [
                        attr.name "session-name"
                        bind.input.string name (dispatch << Message.configInputSessionName)
                    ]

                    div [] [
                        cond target <| function
                            | Ok newName ->
                                button [
                                    attr.type' "button"
                                    attr.class' "btn btn-success btn-sm m-1"
                                    on.click (fun _ -> dispatch <| Message.acceptSessionNameEdit newName)
                                ] [
                                    i [ attr.class' "bi bi-check2-circle" ] []
                                ]
                            | _ ->
                                button [
                                    attr.type' "button"
                                    attr.class' "btn btn-success btn-sm m-1"
                                    attr.disabled true
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

let private canMove tryGetPlayer state player =
    (player <> (state.Config.ConfigPlayers |> PlayerList.configHost)) &&
    state.Config.ConfigPlayers
    |> PlayerList.configPlayers
    |> tryGetPlayer
    |> Option.map ((<>) player)
    |> Option.defaultValue false

let private canMoveUp = canMove List.tryHead

let private canMoveDown = canMove List.tryLast

let private playerItem state (player : Player) name dispatch =
    div [ attr.class' "d-flex flex-row align-items-center justify-content-between" ] [
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
                attr.class' "btn btn-secondary btn-sm m-1"
                attr.disabled (player |> canMoveUp state |> not)
                on.click (fun _ -> dispatch <| Message.movePlayerUp player.Id)
            ] [
                i [ attr.class' "bi bi-arrow-up" ] []
            ]

            button [
                attr.type' "button"
                attr.class' "btn btn-secondary btn-sm m-1"
                attr.disabled (player |> canMoveDown state |> not)
                on.click (fun _ -> dispatch <| Message.movePlayerDown player.Id)
            ] [
                i [ attr.class' "bi bi-arrow-down" ] []
            ]

            button [
                attr.type' "button"
                attr.class' "btn btn-danger btn-sm m-1"
                attr.disabled (player.Id = (state.Config.ConfigPlayers |> PlayerList.configHost).Id)
                on.click (fun _ -> dispatch <| Message.removePlayer player.Id)
            ] [
                i [ attr.class' "bi bi-x-circle" ] []
            ]
        ]
    ]

let private editedPlayerItem editedName target dispatch =
    div [ attr.class' "d-flex flex-row align-items-center justify-content-between" ] [
        input [
            attr.name "player-name"
            bind.input.string editedName (dispatch << Message.configInputPlayerName)
        ]

        div [] [
            cond target <| function
                | Ok newPlayers ->
                    button [
                        attr.type' "button"
                        attr.class' "btn btn-success btn-sm m-1"
                        on.click (fun _ -> dispatch <| Message.acceptPlayerNameEdit newPlayers)
                    ] [
                        i [ attr.class' "bi bi-check2-circle" ] []
                    ]
                | _ ->
                    button [
                        attr.type' "button"
                        attr.class' "btn btn-success btn-sm m-1"
                        attr.disabled true
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

let private players state dispatch =
    section [ attr.class' "col-md-auto m-2 m-md-4" ] [
        h6 [] [
            text "Players"
        ]

        ul [ attr.class' "list-group w-100" ] [
            forEach (state.Config.ConfigPlayers |> PlayerList.configValue) <| fun player ->
                let (PlayerName name) = player.Name

                li [ attr.class' "list-group-item" ] [
                    cond state.EditMode <| function
                        | EditPlayer { Id = playerId; Name = editedName; Target = target } when playerId = player.Id ->
                            editedPlayerItem editedName target dispatch
                        | _ ->
                            playerItem state player name dispatch
                ]
        ]
    ]

let private playerRequests state dispatch =
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

let private chipDistribution state dispatch =
    section [] [
        h6 [ attr.class' "text-center" ] [
            text "Chip Distribution"
        ]

        cond state.Config.ConfigChipDistribution <| function
        | EqualChipDitribution chips ->
            table [ attr.class' "mx-auto"; attr.style "border: none" ] [
                tbody [] [
                    forEach (chips |> Map.toList) <| fun (Chip value as chip, num) ->
                        tr [] [
                            td [ attr.class' "align-self-end" ] [
                                text <| string value
                            ]

                            td [] [
                                input [
                                    attr.name $"chip-{value}-value"
                                    attr.type' "number"
                                    attr.class' "m-1"
                                    bind.input.int num (dispatch << Message.setChipEqualDistributionValue chip)
                                ]
                            ]
                        ]
                ]
            ]
    ]

let private otherSettings state dispatch =
    section [ attr.class' "mt-2" ] [
        h6 [ attr.class' "text-center" ] [
            text "Other Settings"
        ]

        div [ attr.class' "d-flex flex-row justify-content-center" ] [
            p [ attr.class' "m-1" ] [
                text <| "Number of betting rounds:"
            ]

            input [
                attr.class' "m-1"
                attr.name "bet-round-number"
                attr.type' "number"
                bind.input.int state.Config.ConfigBetRoundNumber (dispatch << Message.inputBetRoundNumber)
            ]
        ]
    ]

let configPage js state joinUrl dispatch =
    div [ attr.class' "container" ] [
        header js state joinUrl dispatch
        
        div [ attr.class' "row justify-content-md-center" ] [
            players state dispatch
            playerRequests state dispatch
        ]

        div [ attr.class' "row justify-content-md-center" ] [
            chipDistribution state dispatch
        ]
        
        div [ attr.class' "row justify-content-md-center" ] [
            otherSettings state dispatch
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
