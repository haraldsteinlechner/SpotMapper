open System.IO
open FsCheck
open Expecto


let readTestFile () =
    let pricesFile = Path.Combine(__SOURCE_DIRECTORY__, "..", "assets","prices.csv")
    let usageFile = Path.Combine(__SOURCE_DIRECTORY__, "..", "tests","exampleYearUsages.csv")
    let prices = File.ReadAllLines(pricesFile)
    let usage = File.ReadAllLines(usageFile)
    let prices = SpotMapper.Computation.Parsing.parsePrices prices
    let usage = SpotMapper.Computation.Parsing.parseUsage usage
    prices, usage

let tests =
    testList "Main" [
        test "Hello World" {
            let (prices, failedPrices), (usages, failedUsages) = readTestFile()
            Expect.equal true true ""
        }
    ]


[<EntryPoint>]
let main args =
    let a = [CLIArguments.Debug; CLIArguments.Sequenced]
    runTestsWithCLIArgs a args tests