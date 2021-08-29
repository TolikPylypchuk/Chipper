[<AutoOpen>]
module Chipper.FlowUtil

open FSharpPlus
open Elmish

let pureFlow (a : 'a) : Flow<'a> = monad {
    return a
}

let withNoMessage _ = Message.noMessage

let cmd (cmd : Cmd<Message>) : Flow<unit> = tell [ cmd ]

let asyncCmd (msg : Flow<Async<Message>>) : Flow<unit> = monad {
    let! msg' = msg
    do! cmd <| Cmd.OfAsync.result msg'
}

let continueWithAsync (asyncFlow : Async<Flow<Model>>) : Flow<unit> =
    cmd <| Cmd.OfAsync.perform (fun () -> asyncFlow) () Message.custom

let postEvent id event : Flow<unit> = monad {
    let! mediator = Env.askMediator
    do! cmd <| Cmd.ofSub (fun _ -> mediator |> EventMediator.post event id)
}
