module Chipper.Web.View

open Bolero.Html

let notImplementedPage =
    div [ attr.class' "h-100 d-flex align-items-center justify-content-center" ] [
        h1 [ attr.class' "display-1 text-center" ] [
            text "Hang tight, this one's not implemented yet"
        ]
    ]
