module Chipper.Web.View

open Bolero.Html

let class' = attr.``class``
let type' = attr.``type``

let startPage dispatch =
    concat [
        h1 [ class' "display-1 p-lg-4 p-md-3 p-2 text-center" ] [
            text "Chipper"
        ]

        p [ class' "lead p-2 text-center" ] [
            text "Cards are on you. Chips are on me."
        ]

        div [ class' "text-center p-2" ] [
            button [ type' "button"; class' "btn btn-primary btn-lg"; on.click (fun _ -> dispatch CreateSession) ] [
                text "Create a session"
            ]
        ]
    ]

let notImplemented =
    div [ class' "position-relative vw-100 vh-100" ] [
        div [ class' "container position-absolute top-50 start-50 translate-middle" ] [
            h1 [ class' "display-1 text-center p-4" ] [
                text "Hang tight, this one's not implemented yet"
            ]
        ]
    ]
