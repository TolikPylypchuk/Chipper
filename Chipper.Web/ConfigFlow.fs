module Chipper.Web.ConfigFlow

open FSharpPlus

open FsToolkit.ErrorHandling

open Elmish

open Chipper.Core
open Chipper.Core.Domain
open Chipper.Core.Persistence

let private configureSession config model = monad {
    let! storage = Env.askStorage
    let! repo = Env.askRepo

    let result = asyncResult {
        do! repo |> updateSession (ConfigurableSession config)
        let state = ConfiguringSession { Config = config; PlayerRequests = []; EditMode = NoEdit }
        do! storage.SetState state
        return Message.setModel { model with Page = ConfigurePage; State = state }
    }

    return model, result |> Message.handleAsyncError |> Cmd.OfAsync.result
}

let setBettingType bettingType config =
    configureSession { config with ConfigBettingType = bettingType }

let setRaiseType raiseType config =
    configureSession { config with ConfigRaiseType = raiseType }
