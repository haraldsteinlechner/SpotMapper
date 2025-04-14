#if INTERACTIVE
#else
module SpotMapper.Computation
#endif

open System
open System.Globalization
open System.Collections.Generic

[<Measure>] type h
[<Measure>] type kw
[<Measure>] type kwh = kw * h
[<Measure>] type euro

type SpotInterval = 
    {
        startTime : DateTime
        endTime : DateTime
        price : float<euro/kwh>
    }

type TimeInterval = 
    {
        startTime : DateTime
        endTime : DateTime
    }

type SpotResult = 
    | CostResult of cost : float<euro> * consideredConsumption : float<kwh> * fullConsumption : float<kwh> * matchingPrices : (int * int)
    | CouldNotCompute of string

//let p = getPrice "2024-01-01 00:00:00,2024-01-01 00:15:00,0.10,"
//let u = getUsage "01.01.2024;00:00:00;00:15:00;0,026;;"

let compute (usages : list<TimeInterval * float<kwh>>) 
            (prices : list<TimeInterval * float<euro/kwh>>)
            (constantCost : float<euro/kwh>) =
    
    let withinRange timeInterval (priceInterval, p) = 
        timeInterval.startTime >= priceInterval.startTime && timeInterval.endTime <= priceInterval.endTime

    let startIntervals =
        prices 
        |> Seq.map (fun (t, v) -> t.startTime, (t.endTime, v)) 
        |> Seq.fold (fun acc (k, v) -> 
            match Map.tryFind k acc with
            | None -> Map.add k [v] acc
            | Some xs -> Map.add k (v :: xs) acc) Map.empty


    let spotPrices = 
        usages 
        |> Seq.map (fun (timeInterval, usage) -> 
            match Map.tryFind timeInterval.startTime startIntervals with
            //match Map.tryFind timeInterval prices with
            | Some [(t, price)] -> 
                let price = usage * (price + constantCost)
                Some price
                Some (usage, price)
            | Some _ -> failwith "not implemented"
            | None -> 
                printfn "%A" timeInterval
                None
        )
        
    let totalPrice =    
        spotPrices 
        |> Seq.map (fun x -> 
            match x with
            | Some (_, price) -> price
            | None -> 0.0<euro>)
        |> Seq.sum

    let consideredUsage = 
        spotPrices 
        |> Seq.map (fun x -> 
            match x with
            | Some (usage, _) -> usage
            | None -> 0.0<kwh>)
        |> Seq.sum

    let validMatches = spotPrices |> Seq.filter Option.isSome |> Seq.length
    let overallUsage = usages |> Seq.sumBy (fun (_, usage) -> usage)

    CostResult(totalPrice, consideredUsage, overallUsage, (validMatches, Seq.length usages))


module Parsing =

    open System.Text.RegularExpressions

    type Usages = list<TimeInterval * float<kwh>>
    type Prices = list<TimeInterval * float<euro/kwh>>

    // bit hacky to prevent using tryParseExact
    let parseTimeSpot (s : string) = 
        // Replace the space with 'T' to make it ISO 8601 compatible
        let isoString = s.Replace(" ", "T")
        // Parse the ISO string into a DateTime object
        match DateTime.TryParse(isoString) with
        | (true, v) -> Some v
        | _ -> 
            None


    // bit hacky to prevent using tryParseExact
    let parseTimeUsage (date : string) (time : string) =
        let pattern = @"(\d{2})\.(\d{2})\.(\d{4});(\d{2}):(\d{2}):(\d{2})"
        let matchResult = Regex.Match(date + ";" + time, pattern)
        
        if matchResult.Success then
            // Extract the matched groups
            let day = int matchResult.Groups.[1].Value
            let month = int matchResult.Groups.[2].Value
            let year = int matchResult.Groups.[3].Value
            let hour = int matchResult.Groups.[4].Value
            let minute = int matchResult.Groups.[5].Value
            let second = int matchResult.Groups.[6].Value
            
            Some(DateTime(year, month, day, hour, minute, second))
        else
            None

    let extractDateTimeUsage (input: string) =
        let pattern = @"^(\d{2}\.\d{2}\.\d{4});(\d{2}:\d{2}:\d{2});(\d{2}:\d{2}:\d{2})"
        let matchResult = Regex.Match(input, pattern)
        if matchResult.Success then
            let datePart = matchResult.Groups.[1].Value
            let timePart0 = matchResult.Groups.[2].Value
            let timePart1 = matchResult.Groups.[3].Value
            match parseTimeUsage datePart timePart0, parseTimeUsage datePart timePart1 with
            | Some v0, Some v1 -> 
                // printfn "%A %A" v0 v1
                Some (v0, v1)
            | _ -> None
        else
            None

    let getPrice (s : string) =
        //2024-01-01 00:00:00,2024-01-01 00:15:00,0.10,
        match s.Split(',') with
        | [|st; et; p;_|] -> 
            match parseTimeSpot st, parseTimeSpot et with
            | Some st, Some et -> 
                let price = System.Double.Parse(p, CultureInfo.InvariantCulture) * 0.001<euro/kwh>
                ({startTime = st; endTime = et; }, price) |> Some
            | _ -> 
                None
        | _ -> 
            None

    let getUsage (s : string) = 
        //01.01.2024;00:00:00;00:15:00;0,026;;
        match s.Split(';'), extractDateTimeUsage s with
        | [|_;_;ts;p;_;_|], Some (st, et) -> 
            let s = p.Replace(",",".")
            let usage = System.Double.Parse(s, CultureInfo.InvariantCulture)
            ({startTime = st; endTime = et  }, usage * 1.0<kwh>) |> Some
        | _ -> 
            None

    let private parseLines (csvLines : array<string>) (f : string -> Option<TimeInterval *'a>) = 
        let lines = 
            csvLines
            |> Array.skip 1
            |> Array.map f

        let results, failed =
            let f (xs, c) =
                function 
                    | None -> (xs, c + 1) 
                    | Some x -> (x::xs, c)

            lines 
            |> Array.fold f ([], 0)
       

        results |> List.rev, failed


    let parsePrices (csvLines : array<string>) = 
        parseLines csvLines getPrice

    let parseUsage (csvLines : array<string>) = 
        parseLines csvLines getUsage


#if FABLE_COMPILER
#else
module Testing = 

    open System.IO

    let readTestFile () =
        let pricesFile = Path.Combine(__SOURCE_DIRECTORY__, "..", "public","prices.csv")
        let usageFile = Path.Combine(__SOURCE_DIRECTORY__, "..", "tests","exampleYearUsages.csv")
        let prices = File.ReadAllLines(pricesFile)
        let usage = File.ReadAllLines(usageFile)
        let prices = Parsing.parsePrices prices
        let usage, r = Parsing.parseUsage usage
        printfn "%d" (List.length usage)
        prices, (usage,r )


    let myTest =
        let (prices, failedPrices), (usages, failedUsages) = readTestFile()
        compute usages prices (0.0<euro/kwh> / 100.0)

#endif