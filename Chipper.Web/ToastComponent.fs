namespace Chipper.Web

open System.Threading.Tasks

open Microsoft.AspNetCore.Components
open Microsoft.JSInterop

open Bolero
open Bolero.Html

open Chipper.Core.Domain

type Toast<'model, 'message> = {
    Header : 'model -> Node
    Body : 'model -> Node
    AcceptText : string
    CancelText : string
    OnAccept : 'message
    OnCancel : 'message
    OnClose : 'message
}

[<RequireQualifiedAccess>]
module Toast =

    let localState =
        {
            Header = fun _ -> text "Hi there!"
            Body =
                function
                | ConfiguringSession { ConfigName = GameSessionName sessionName } ->
                    text <| sprintf
                        "It appears that you were previously configuring a game: %s. Would you like to continue?"
                        sessionName
                | _ ->
                    text "Uh, I got confused and showed you this message by mistake. Oops"

            AcceptText = "Continue"
            CancelText = "Forget it"

            OnAccept = RecoverLocalState
            OnCancel = ClearLocalState
            OnClose = IgnoreLocalState
        }

type ToastComponent<'model, 'message> () =
    inherit ElmishComponent<'model, 'message> ()

    let toastRef = HtmlRef()

    [<Inject>]
    member val JS = Unchecked.defaultof<IJSRuntime> with get, set

    [<Parameter>]
    member val Toast = Unchecked.defaultof<Toast<'model, 'message>> with get, set
    
    override this.View model dispatch =
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
                        this.Toast.Header model
                    ]

                    button [
                        attr.type' "button"
                        attr.class' "btn-close"
                        attr.bs "dismiss" "toast"
                        attr.aria "label" "Close"
                        on.click (fun _ -> dispatch this.Toast.OnClose)
                    ] []
                ]

                div [ attr.class' "toast-body" ] [
                    this.Toast.Body model

                    div [ attr.class' "mt-2 pt-2 border-top" ] [
                        button [
                            attr.type' "button"
                            attr.class' "btn btn-primary btn-sm"
                            on.click (fun _ -> dispatch this.Toast.OnAccept) ] [
                            text this.Toast.AcceptText
                        ]

                        button [
                            attr.type' "button"
                            attr.class' "btn btn-secondary btn-sm ms-2"
                            attr.bs "dismiss" "toast"
                            on.click (fun _ -> dispatch this.Toast.OnCancel) ] [
                            text this.Toast.CancelText
                        ]
                    ]
                ]
            ]
        ]

    override this.OnAfterRenderAsync firstRender =
        if firstRender
        then this.JS.InvokeVoidAsync("showToast", toastRef.Value).AsTask()
        else Task.CompletedTask

[<RequireQualifiedAccess>]
module ToastComponent =

    let localState =
        let tc = Unchecked.defaultof<ToastComponent<_, _>>
        ecomp<ToastComponent<LocalState, Message>, LocalState, Message> [ (nameof tc.Toast) => Toast.localState ]
