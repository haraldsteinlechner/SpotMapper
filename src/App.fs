module App

open Feliz
open Elmish
open Feliz.Bulma

open SpotMapper.Computation


module FileUpload =

    open Fable.Core.JsInterop
    let handleFileEvent onLoad (fileEvent:Browser.Types.Event) =
        let files:Browser.Types.FileList = !!fileEvent.target?files
        if files.length > 0 then
            let reader = Browser.Dom.FileReader.Create()
            reader.onload <- (fun _ -> reader.result |> unbox |> onLoad)
            reader.readAsText(files.[0])
 
    let createFileUpload onLoad =
        Html.input [ 
            prop.className "file input"
            prop.type' "file"
            prop.label "Choose a file"
            prop.onChange (handleFileEvent onLoad)
        ]

    let createFileUpload2 onLoad =
        Html.div [
            prop.className "file"
            prop.children [
                Html.label [
                    prop.className "file-label"
                    prop.children [
                        Html.input [
                            prop.className "file-input"
                            prop.type' "file"
                            prop.name "resume"
                            prop.onChange (handleFileEvent onLoad)
                        ]
                        Html.span [
                            prop.className "file-cta"
                            prop.children [
                                Html.span [
                                    prop.className "file-icon"
                                    prop.children [
                                        Html.i [
                                            prop.classes [ "fas"; "fa-upload" ]
                                        ]
                                    ]
                                ]
                                Html.span [
                                    prop.className "file-label"
                                    prop.text " Choose a file…"
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


type State = { 
    Count: int; 
    cost : Option<float<euro>>;
    usage: Option<string>
    prices: Option<string> 
}

type Msg =
    | Increment
    | Decrement
    | SetUsage of string
    | SetPrices of string


let init() = 
    let f = 
        Cmd.OfAsync.perform (fun () -> 
            async {
                let! (i,r) = Fable.SimpleHttp.Http.get "./prices.csv"
                return r
            }
        ) () SetPrices
    { Count = 0; usage = None; prices = None; cost = None }, f

let updateCost (s : State) =
    match s.usage, s.prices with
    | Some usage, Some prices ->
        let cost = SpotMapper.Computation.compute usage prices
        { s with cost = Some cost }
    | _ -> s

let update (msg: Msg) (state: State) =
    match msg with
    | Increment -> { state with Count = state.Count + 1 }, Cmd.none
    | Decrement -> { state with Count = state.Count - 1 }, Cmd.none
    | SetUsage usage -> 
        { state with usage = Some usage} |> updateCost, Cmd.none
    | SetPrices prices ->
        { state with prices = Some prices} |> updateCost, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
    Bulma.container [
        Bulma.section [
            Bulma.title.h1 "Spot Price Calculator"
        ]
        Bulma.section [
            Bulma.field.div [
                Bulma.label "Upload Usage File for 2024"
                Bulma.control.div [
                    FileUpload.createFileUpload2 (dispatch << SetUsage)
                ]
            ]
        
            (*Bulma.field.div [
                Bulma.label "Upload Prices File"
                Bulma.control.div [
                    FileUpload.createFileUpload (dispatch << SetPrices)
                ]
            ]*)
        ]

        Bulma.section [
            match state.cost with
            | None -> Bulma.subtitle.h1 "No cost calculated yet"
            | Some cost ->
                Bulma.label (sprintf "Working price on spot market 2024: %.2f€" cost)
        ]
    ]