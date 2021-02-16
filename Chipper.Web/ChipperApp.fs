module Chipper.Web.ChipperApp

open Microsoft.Extensions.DependencyInjection

open Elmish
open Flurl
open Bolero

open Chipper.Core.Domain
open Chipper.Core.Persistence
open Chipper.Web.Workflows

let init storage repo =
    let model = { Page = HomePage; State = NotLoaded }
    let cmd = Cmd.OfAsync.either (fun () -> getState storage repo) () SetInitialState SetException

    model, cmd

let update storage repo message model =
    let doNothing = model, Cmd.none
    match message, model.State with

    | SetInitialState state, _ -> { model with State = state }, Cmd.none

    | SetPage page, _ -> { model with Page = page }, Cmd.none

    | SetModel model, _ -> model, Cmd.none

    | _, NotLoaded -> doNothing

    | SetError e, _ ->
        printfn "An unhandled error appeared: %O" e
        doNothing

    | SetException e, _ ->
        printfn "%O" e.Message
        doNothing
        
    | StartGameSession, _ ->
        { model with Page = StartPage; State = AddingSessionName "" }, Cmd.none

    | InputSessionName name, _ ->
        { model with State = AddingSessionName name }, Cmd.none

    | SaveSessionName, AddingSessionName name ->
        model, Cmd.OfAsync.result <| startNewSession storage repo model name
        
    | SaveSessionName, _ ->
        doNothing

    | ConfigureGameSession, _ ->
        doNothing

let view createJoinUrl model dispatch =
    match model.Page, model.State with
    | _, NotLoaded -> Empty
    | HomePage, _ -> View.homePage dispatch
    | StartPage, AddingSessionName name -> View.startPage (name |> Model.canSaveName) dispatch
    | InvitePage, StartingSession newSession -> View.invitePage (createJoinUrl newSession.Id) dispatch
    | _ -> View.notImplementedPage

type AppComponent() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let settings = this.Services.GetRequiredService<AppSettings>()
        let repo = this.Services.GetRequiredService<GameSessionRepository>()
        let storage = this.Services.GetRequiredService<LocalStorage>()

        let createJoinUrl = fun (GameSessionId id) -> Url.Combine(settings.UrlRoot, (router.Link <| JoinPage id))

        let init _ = init storage repo
        let update = update storage repo
        let view = view createJoinUrl

        Program.mkProgram init update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withConsoleTrace
#endif
