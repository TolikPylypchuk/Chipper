namespace Chipper.Web

open Bolero

type Page =
    | [<EndPoint("/")>] StartPage
    | [<EndPoint("/not-implemented")>] NotImplemented

type Model = {
    Page : Page
}
