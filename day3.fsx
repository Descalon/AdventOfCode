#load "Data.fsx"
#r "nuget:Expecto"
open System
open Expecto

let testdata = ["00100";"11110";"10110";"10111";"10101";"01111";"00111";"11100";"10000";"11001";"00010";"01010";]

let charToInt (c:char) = c |> string |> int
let dropHead (s:string) = s[1..s.Length-1]
let at (index:int32) (s:string) = s[index]
let head = at 0
let convertFromBinaryString s = System.Convert.ToInt32(s,2)

let findMostCommon xs =
    let sum = xs |> List.filter ((=) '0') |> List.length
    let check = (List.length xs) / 2
    if sum > check then '0' else '1' 

let negateChar = function
| '0' -> '1'
| '1' -> '0'
| _ -> '0'

let negate (s:string) = s |> Seq.map negateChar |> String.Concat

let processData =
    let rec fn acc (data: string list) =
        match data with
        | [] -> acc | x::_ when x.Length = 0 -> acc
        | _ ->
            let h = data |> List.map head |> findMostCommon |> string
            let acc' = acc + h
            fn acc' (List.map dropHead data)
    fn "" 

let calc a b = convertFromBinaryString a * convertFromBinaryString b

let parseTestResult = processData testdata
let expectedParseResult = "10110"
Expect.equal parseTestResult expectedParseResult "Parse test failed"

let negateTestResult = negate parseTestResult
let expectedNegateResult = "01001"
Expect.equal negateTestResult expectedNegateResult "Negate test failed"

let calculationTestResult = calc parseTestResult negateTestResult
let expectedCalculationResult = 198
Expect.equal calculationTestResult expectedCalculationResult "Calculation test failed"

let calculate xs =
    let a = processData xs
    let b = negate a
    calc a b

printfn "Result of day 3: %i" (calculate <| Data.powerData())

let processData' cfn =
    let rec fn i (data: string list) =
        match data with
        | [x] -> x 
        | _ ->
            let h = data |> List.map (at i) |> findMostCommon
            let data' = data |> List.filter ((at i) >> (cfn) h)
            fn (i+1) data'
    fn 0

let processOxygenRating = processData' (=)
let processCO2Rating = processData' (<>)

let oxygenRatingTestResult = processOxygenRating testdata
let expectedOxygenRating = "10111"
Expect.equal oxygenRatingTestResult expectedOxygenRating "Oxygen rating test failed"

let co2RatingTestResult = processCO2Rating testdata
let expectedCO2Rating = "01010"
Expect.equal co2RatingTestResult expectedCO2Rating "CO2 rating test failed"

let calculationTestResult2 = calc oxygenRatingTestResult co2RatingTestResult
let expectedCalculationResult2 = 230
Expect.equal calculationTestResult2 expectedCalculationResult2 "Second calculation test failed"

let calculate' xs =
    let a = processOxygenRating xs
    let b = processCO2Rating xs
    calc a b

printfn "Result of day 3 part 2: %i" (calculate' <| Data.powerData())