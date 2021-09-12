module Chipper.PlayFlow

open FSharpPlus

open FsToolkit.ErrorHandling

open Elmish

let startGame (state : GameSessionState) model : Flow<Model> = monad {
    let! storage = Env.askStorage
    let! repo = Env.askRepo
    let! mediator = Env.askMediator

    let result = asyncResult {
        let! gameId = repo.GenerateGameId ()
        let! game, gameSession = state.GameSession |> Domain.newGame gameId state.Player
        let localState = AwaitingTurn { Game = game; GameSession = gameSession; Player = state.Player }

        do! storage.SetState localState

        mediator |> EventMediator.post (GameStarted (game, state.Player)) (gameSession |> GameSession.id)
        
        do! repo |> Persistence.updateSession (PersistentSession gameSession)

        return Message.setModel { model with Page = PlayPage; State = localState }
    }

    do! cmd (result |> Message.handleAsyncError |> Cmd.OfAsync.result)

    return model
}
