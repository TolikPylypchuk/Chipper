module Chipper.Web.LocalStorage

open Blazored.LocalStorage

let getLocalState (localStorage : ILocalStorageService) () =
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
