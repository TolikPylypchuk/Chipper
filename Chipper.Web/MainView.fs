module Chipper.Web.MainView

open Microsoft.Extensions.Hosting

open Bolero.Html
open Bolero.Server.Html

type HostingEnvirontmentType =
    | Development
    | Staging
    | Production
    | Other of string

let private envType (env : IHostEnvironment) =
    if env.IsProduction() then
        Production
    elif env.IsStaging() then
        Staging
    elif env.IsDevelopment() then
        Development
    else
        Other env.EnvironmentName

let html (env : IHostEnvironment) =
    doctypeHtml [ attr.lang "en" ] [
        head [] [
            meta [ attr.charset "utf-8" ]
            meta [ attr.name "viewport"; attr.content "width=device-width, initial-scale=1.0" ]
            meta [ attr.name "msapplication-TileColor"; attr.content "#ffffff" ]
            meta [ attr.name "msapplication-TileImage"; attr.content "/ms-icon-144x144.png" ]
            meta [ attr.name "theme-color"; attr.content "#ffffff" ]

            ``base`` [ attr.href "/" ]

            title [] [ text "Chipper" ]

            link [ attr.rel "apple-touch-icon"; attr.sizes "57x57"; attr.href "/apple-icon-57x57.png" ]
            link [ attr.rel "apple-touch-icon"; attr.sizes "60x60"; attr.href "/apple-icon-60x60.png" ]
            link [ attr.rel "apple-touch-icon"; attr.sizes "72x72"; attr.href "/apple-icon-72x72.png" ]
            link [ attr.rel "apple-touch-icon"; attr.sizes "76x76"; attr.href "/apple-icon-76x76.png" ]
            link [ attr.rel "apple-touch-icon"; attr.sizes "114x114"; attr.href "/apple-icon-114x114.png" ]
            link [ attr.rel "apple-touch-icon"; attr.sizes "120x120"; attr.href "/apple-icon-120x120.png" ]
            link [ attr.rel "apple-touch-icon"; attr.sizes "144x144"; attr.href "/apple-icon-144x144.png" ]
            link [ attr.rel "apple-touch-icon"; attr.sizes "152x152"; attr.href "/apple-icon-152x152.png" ]
            link [ attr.rel "apple-touch-icon"; attr.sizes "180x180"; attr.href "/apple-icon-180x180.png" ]

            link [ attr.rel "icon"; attr.type' "image/png"; attr.sizes "16x16"; attr.href "/favicon-16x16.png" ]
            link [ attr.rel "icon"; attr.type' "image/png"; attr.sizes "32x32"; attr.href "/favicon-32x32.png" ]
            link [ attr.rel "icon"; attr.type' "image/png"; attr.sizes "96x96"; attr.href "/favicon-96x96.png" ]
            link [
                attr.rel "icon"; attr.type' "image/png"; attr.sizes "192x192"; attr.href "/android-icon-192x192.png"
            ]

            link [ attr.rel "manifest"; attr.href "/manifest.json" ]

            link [ attr.rel "stylesheet"; attr.href "/lib/bootstrap/dist/css/bootstrap.min.css" ]
            link [ attr.rel "stylesheet"; attr.href "/lib/bootstrap-icons/font/bootstrap-icons.min.css" ]
            link [ attr.rel "stylesheet"; attr.href "/css/index.css" ]
        ]

        body [ attr.class' "vw-100 vh-100" ] [
            rootComp<ChipperApp.AppComponent>

            div [ attr.id "blazor-error-ui" ] [
                cond (envType env) <| function
                | Staging | Production ->
                    text "An error has occurred. This application may no longer respond until reloaded."
                | Development ->
                    text "An unhandled exception has occurred. See browser dev tools for details."
                | _ ->
                    empty

                a [ attr.href ""; attr.class' "reload" ] [
                    text "Reload"
                ]
                
                a [ attr.class' "dismiss" ] [
                    text "🗙"
                ]
            ]

            boleroScript
            script [ attr.src "/lib/bootstrap/dist/js/bootstrap.min.js" ] []
            script [ attr.src "/js/show-toast.js" ] []
        ]
    ]
