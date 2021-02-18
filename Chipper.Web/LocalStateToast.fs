namespace Chipper.Web

open System.Threading.Tasks

open Microsoft.AspNetCore.Components
open Microsoft.JSInterop

open Bolero
open Bolero.Html

open Chipper.Core.Domain

type LocalStateToast () =
    inherit ElmishComponent<LocalState, Message> ()

    let toastRef = HtmlRef()

    [<Inject>]
    member val JS = Unchecked.defaultof<IJSRuntime> with get, set

    override _.View model dispatch =
        div [ attr.class' "position-fixed bottom-0 end-0 p-md-3 p-2" ] [
            div [
                attr.ref toastRef
                attr.class' "toast"
                attr.role "alert"
                attr.bs "autohide" "false"
                attr.aria "live" "assertive"
                attr.aria "atomic" true
            ] [
                div [ attr.class' "toast-header" ] [
                    strong [ attr.class' "me-auto" ] [
                        text "Hi there!"
                    ]

                    button [
                        attr.type' "button"
                        attr.class' "btn-close"
                        attr.bs "dismiss" "toast"
                        attr.aria "label" "Close"
                    ] []
                ]

                div [ attr.class' "toast-body" ] [
                    match model with
                    | StartingSession { Name = GameSessionName sessionName } ->
                        text <| sprintf
                            "It appears that you were previously creating a game: %s. Would you like to continue?"
                            sessionName
                    | _ ->
                        text "Uh, I got confused and showed you this message by mistake. Oops"

                    div [ attr.class' "mt-2 pt-2 border-top" ] [
                        button [
                            attr.type' "button"
                            attr.class' "btn btn-primary btn-sm"
                            on.click (fun _ -> dispatch RecoverLocalState) ] [
                            text "Continue"
                        ]
                        button [
                            attr.type' "button"
                            attr.class' "btn btn-secondary btn-sm ms-2"
                            attr.bs "dismiss" "toast"
                            on.click (fun _ -> dispatch ClearLocalState) ] [
                            text "Forget It"
                        ]
                    ]
                ]
            ]
        ]

    override this.OnAfterRenderAsync firstRender =
        if firstRender
        then this.JS.InvokeVoidAsync("showToast", toastRef.Value).AsTask()
        else Task.CompletedTask
