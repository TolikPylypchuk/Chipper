namespace Chipper.Web

open Bolero

type Message =
    | SetPage of Page
    | CreateSession
