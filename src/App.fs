module SpotMapper.App

open System
open Feliz
open Elmish
open Feliz.Bulma
open Feliz.Plotly

open SpotMapper.Computation
open Fable.Core.JsInterop

module FileUpload =

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
    observedDay: Option<DateTime>; 
    cost : Option<float<euro>>
    usage: Option<Parsing.Usages>
    prices: Option<Parsing.Prices> 
}

type Msg =
    | SetUsage of string
    | SetPrices of Option<string>
    | SetDate of string


let init() = 
    let f = 
        Cmd.OfAsync.perform (fun () -> 
            async {
                let! (i, r) = Fable.SimpleHttp.Http.get "./prices.csv"
                if i <> 200 then 
                    return None
                else
                    return Some r
            }
        ) () SetPrices
    { observedDay = Some (DateTime(2024, 6, 16)); usage = None; prices = None; cost = None }, f

let updateCost (s : State) =
    match s.usage, s.prices with
    | Some usage, Some prices ->
        let constant = 1.6<euro/kwh> / 100.0
        match SpotMapper.Computation.compute usage prices constant with 
        | SpotResult.CostResult (cost, _, _) ->
            { s with cost = Some cost }
        | _ -> 
            { s with cost = None }
    | _ -> s

let strToCsv (str : string) =
    str.Split([|'\n'|], System.StringSplitOptions.RemoveEmptyEntries)

let update (msg: Msg) (state: State) =
    match msg with
    | SetDate date ->
        match DateTime.TryParse date with
        | true, d -> { state with observedDay = Some d }, Cmd.none
        | false, _ -> state, Cmd.none
    | SetUsage usage -> 
        match SpotMapper.Computation.Parsing.parseUsage (strToCsv usage) with
        | (usages, parsedResults) ->
            { state with usage = Some usages } |> updateCost, Cmd.none
    | SetPrices prices ->
        match prices with
        | Some prices ->
            let p, _ =  Parsing.parsePrices (strToCsv prices)
            { state with prices =  Some p } 
            |> updateCost, Cmd.none
        | _ -> state, Cmd.none

let renderPricesGraph (state: State) =
    match state.observedDay, state.prices with
    | Some selectedDate, Some prices ->
        // Filter prices data for the selected date
        let filteredPrices =
            prices
            |> List.filter (fun (timeInterval, _) ->
                timeInterval.startTime.Date = selectedDate.Date
            )

        // Extract time intervals and price values
        let times =
            filteredPrices
            |> List.map (fun (timeInterval, _) -> timeInterval.startTime.ToString("HH:mm"))

        let values =
            filteredPrices
            |> List.map (fun (_, priceValue) -> float priceValue)

        // Render the Plotly graph
        Plotly.plot [
            plot.traces [
                traces.scatter [
                    scatter.x times
                    scatter.y values
                    scatter.mode [ scatter.mode.lines; scatter.mode.markers ]
                    scatter.name "Prices (€/kWh)"
                ]
            ]
            plot.layout [
                layout.title "Prices for Selected Date"
                layout.xaxis [
                    xaxis.title "Time"
                ]
                layout.yaxis [
                    yaxis.title "Price (€/kWh)"
                ]
            ]
        ]
    | _ ->
        // Show a message if no data is available
        Html.div [
            Html.h2 "No price data available for the selected date."
        ]

let renderUsageGraph (state: State) =
    match state.observedDay, state.usage with
    | Some selectedDate, Some usage ->
        // Filter usage data for the selected date
        let filteredUsage =
            usage
            |> List.filter (fun (timeInterval, _) ->
                timeInterval.startTime.Date = selectedDate.Date
            )

        // Extract time intervals and usage values
        let times =
            filteredUsage
            |> List.map (fun (timeInterval, _) -> timeInterval.startTime.ToString("HH:mm"))

        let values =
            filteredUsage
            |> List.map (fun (_, usageValue) -> float usageValue)

        // Render the Plotly graph
        Plotly.plot [
            plot.traces [
                traces.scatter [
                    scatter.x times
                    scatter.y values
                    scatter.mode [ scatter.mode.lines; scatter.mode.markers ]
                    scatter.name "kWh Usage"
                ]
            ]
            plot.layout [
                layout.title "kWh Usage for Selected Date"
                layout.xaxis [
                    xaxis.title "Time"
                ]
                layout.yaxis [
                    yaxis.title "kWh"
                ]
            ]
        ]
    | _ ->
        // Show a message if no data is available
        Html.div [
            Html.h2 "No usage data available for the selected date."
        ]

let render (state: State) (dispatch: Msg -> unit) =
    Bulma.container [
        Bulma.section [
            Bulma.title.h1 "Spot Price Calculator"
        ]
        Bulma.section [
            Bulma.field.div [
                Bulma.label "Provide Usage File for 2024 (csv export)"
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
            | None -> 
                Html.div [ 
                    prop.style [style.color.red]
                    prop.children [
                        Html.text "No cost calculated yet (provide a csv file above)"
                    ]
                ]
            | Some cost ->
                Bulma.label (sprintf "Working price on spot market 2024: %.2f€" cost)
        ]
      
        Bulma.section [
            Bulma.field.div [
                Bulma.label "Select a Date"
                Bulma.control.div [
                    Html.input [
                        prop.type' "date"
                        prop.className "input"
                        match state.observedDay with
                        | None -> ()
                        | Some b -> 
                            prop.value (b.ToString("yyyy-MM-dd"))
                        prop.onChange (fun (ev : Browser.Types.Event) -> 
                            dispatch (SetDate ev.target?value)
                        )
                    ]
                ]
            ]
        ]
        Bulma.section [
            match state.observedDay with
            | None -> Bulma.subtitle.h2 "No date selected"
            | Some date ->
                Bulma.subtitle.h2 (sprintf "Selected Date: %s" (date.ToString("yyyy-MM-dd")))

            renderUsageGraph state
            renderPricesGraph state
        ]
    ]