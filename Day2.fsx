#load "Data.fsx"
#load "Utils.fsx"
#r "nuget:Expecto"

open System
open Expecto

type RPS = Rock | Paper | Scissors
type WDL = Win | Draw | Loss
let testdata = ["A","Y";"B","X";"C","Z";"A","Y";"B","X";"C","Z";"A","Y";"B","X";"C","Z";"A","Y";"B","X";"C","Z";]

let print p = Utils.printResult 4 p

let parse = function
| "A" | "X" -> Rock
| "B" | "Y" -> Paper
| "C" | "Z" -> Scissors
| _ -> failwith "THE HELL?"

let opParse = function
| Rock -> "A"
| Paper -> "B"
| Scissors -> "C"

let mineParse = function
| Rock -> "X"
| Paper -> "Y"
| Scissors -> "Z"

let parsePair (a,b) = parse a, parse b

let scoreRPS = function
| Rock -> 1
| Paper -> 2
| Scissors -> 3

let scoreWDL = function
| Loss -> 0
| Draw -> 3
| Win -> 6

let play (mine: RPS) (op: RPS)=
    match mine with 
    | Rock -> match op with | Rock -> Draw | Paper -> Loss | Scissors -> Win
    | Paper -> match op with | Rock -> Win | Paper -> Draw | Scissors -> Loss
    | Scissors -> match op with | Rock -> Loss | Paper -> Win | Scissors -> Draw

let calc (mine,op) = 
    let s = scoreRPS mine
    let p = play mine op
    (scoreWDL p) + s

let processData = List.map (parsePair >> calc) >> List.sum

let debug mine op = 
    let s = scoreRPS mine
    let p = play mine op
    let w = (scoreWDL p) + s
    sprintf "%i - %O - %O <> %O" w p (mineParse mine) (opParse op)

// let testResult = testdata |> processData
// let expectedResult = 15
// Expect.equal testResult expectedResult "Test result is incorrect"

let write (input: string list) = System.IO.File.AppendAllLines("./foo.txt", input)

Data.rpsData() |> List.map (parsePair) |> List.map (fun (a,b) -> debug b a) |> write

Data.rpsData() |> processData |> print 1
testdata |> processData |> print 1