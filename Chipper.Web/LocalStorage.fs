namespace Chipper.Web

type GetLocalState = unit -> Async<LocalState>
type SetLocalState = LocalState -> Async<unit>
type ClearLocalState = unit -> Async<unit>

type LocalStorage = {
    GetState : GetLocalState
    SetState : SetLocalState
    ClearState : ClearLocalState
}

[<RequireQualifiedAccess>]
module LocalStorage =

    let getState storage = storage.GetState ()

    let setState state storage = storage.SetState state

    let clearState storage = storage.ClearState ()
