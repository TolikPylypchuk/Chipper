namespace Chipper.Web

open Blazored.LocalStorage

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

    let getLocalState (localStorage : ILocalStorageService) =
        async {
            let! containsState = await <| localStorage.ContainKeyAsync(nameof LocalState)
            if containsState then
                try return! await <| localStorage.GetItemAsync<LocalState>(nameof LocalState)
                with e ->
                    printfn "%O" e
                    return NoState
            else
                return NoState
        }

    let setLocalState (localStorage : ILocalStorageService) (state : LocalState) =
        await' <| localStorage.SetItemAsync(nameof LocalState, state)

    let clearLocalState (localStorage : ILocalStorageService) =
        await' <| localStorage.RemoveItemAsync(nameof LocalState)
