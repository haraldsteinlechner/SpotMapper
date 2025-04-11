module SpotMapper.Computation

open System
open System.Globalization

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


let getPrice (s : string) =
    //2024-01-01 00:00:00,2024-01-01 00:15:00,0.10,
    match s.Split(',') with
    | [|st;et;p;_|] -> 
        //let startTime = DateTime.ParseExact(st, "yyyy-MM-dd HH:mm:ss", CultureInfo.InvariantCulture)
        //let endTime = DateTime.ParseExact(et, "yyyy-MM-dd HH:mm:ss", CultureInfo.InvariantCulture)
        let price = System.Double.Parse(p, CultureInfo.InvariantCulture) * 0.001<euro/kwh>
        //let startTime = DateTime.ParseExact(st, "yyyy-MM-dd HH:mm:ss", CultureInfo.InvariantCulture)
        { startTime = DateTime.Now; endTime = DateTime.Now; price = price }
    | _ -> 
        failwithf "Invalid format: %s" s

let getUsage (s : string) = 
    //01.01.2024;00:00:00;00:15:00;0,026;;
    match s.Split(';') with
    | [|st;et;_us;_p;_;_|] -> 
        //let startTime = DateTime.ParseExact(st, "dd.MM.yyyy HH:mm:ss", CultureInfo.InvariantCulture)
        //let endTime = DateTime.ParseExact(et, "dd.MM.yyyy HH:mm:ss", CultureInfo.InvariantCulture)
        let s = _p.Replace(",",".")
        let usage = System.Double.Parse(s, CultureInfo.InvariantCulture)
        usage * 1.0<kwh>
    | _ -> 
        failwithf "Invalid format: %s" s

//let p = getPrice "2024-01-01 00:00:00,2024-01-01 00:15:00,0.10,"
//let u = getUsage "01.01.2024;00:00:00;00:15:00;0,026;;"

let compute (allUsages : string) (allPrices : string) =
    let pricesIntervals = 
        allPrices.Split(Environment.NewLine) |> Seq.skip 1
    let usageIntervals = 
        allUsages.Split(Environment.NewLine) |> Seq.skip 1


    let cost = 
        Seq.zip pricesIntervals usageIntervals
        |> Seq.map (fun (priceLine, usageLine) -> 
            let price = getPrice priceLine
            let getUsage = getUsage usageLine
            let workingPrice = price.price + (1.9 * 0.01<euro/kwh>)
            let r = workingPrice * getUsage
            r
        )
        |> Seq.sum

    cost

