module Chipper.Web.PlayView

open Bolero.Html

open Chipper

let playPage state dispatch =
    concat [
        section [] [
            text (state.GameSession |> GameSession.name |> GameSessionName.value)
        ]
    
        section [] [
            text (state.Player.Name |> PlayerName.value)
        ]
    ]
