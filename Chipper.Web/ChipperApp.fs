module Chipper.Web.ChipperApp

open System
open Microsoft.Extensions.DependencyInjection
open Microsoft.JSInterop

open FSharp.Control.Reactive

open Elmish
open Flurl
open Bolero

open Chipper.Core.Domain
open Chipper.Core.Persistence
open Chipper.Web.Workflows

let private inputSubject = Subject.behavior ""

let private inputDebouncer = inputSubject |> Observable.throttle (TimeSpan.FromMilliseconds(300.0))

let cmdDebounceInput message x =
    Cmd.ofSub (fun dispatch ->
        inputSubject
        |> Subject.onNext x
        |> ignore

        inputDebouncer
        |> Observable.take 1
        |> Observable.map DebounceEnd
        |> Observable.map message
        |> Observable.subscribe dispatch
        |> ignore)

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
        
    | InputSessionName (DebounceStart name), _ ->
        model, name |> cmdDebounceInput InputSessionName

    | InputSessionName (DebounceEnd name), _ ->
        { model with State = AddingSessionName name }, Cmd.none

    | SaveSessionName, AddingSessionName name ->
        model, Cmd.OfAsync.result <| startNewSession storage repo model name
        
    | SaveSessionName, _ ->
        doNothing

    | ConfigureGameSession, _ ->
        doNothing

let view js createJoinUrl model dispatch =
    match model.Page, model.State with
    | _, NotLoaded -> Empty
    | HomePage, _ -> View.homePage dispatch
    | StartPage, AddingSessionName name -> View.startPage (name |> Model.canSaveName) dispatch
    | InvitePage, StartingSession newSession -> View.invitePage js (createJoinUrl newSession.Id) dispatch
    | _ -> View.notImplementedPage

type AppComponent() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let settings = this.Services.GetRequiredService<AppSettings>()
        let repo = this.Services.GetRequiredService<GameSessionRepository>()
        let storage = this.Services.GetRequiredService<LocalStorage>()
        let js = this.Services.GetRequiredService<IJSRuntime>()

        let createJoinUrl = fun (GameSessionId id) -> Url.Combine(settings.UrlRoot, (router.Link <| JoinPage id))

        let init _ = init storage repo
        let update = update storage repo
        let view = view js createJoinUrl

        Program.mkProgram init update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withConsoleTrace
#endif
