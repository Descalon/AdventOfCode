#load "Data.fsx"
#load "Utils.fsx"
#r "nuget:Expecto"

open Expecto
open System
open Data
open Utils

let print (result: int) part = printResult 2 part result

let data =
    [ [ 7; 6; 4; 2; 1 ]
      [ 1; 2; 7; 8; 9 ]
      [ 9; 7; 6; 2; 1 ]
      [ 1; 3; 2; 4; 5 ]
      [ 8; 6; 4; 4; 1 ]
      [ 1; 3; 6; 7; 9 ] ]

let isSafe (report: int list) =
    let inc =
        [ for i in 0 .. report.Length - 2 do
              report[i + 1] - report[i] ]
        |> Set.ofList

    Set.isSubset inc ([ 1; 2; 3 ] |> Set.ofList)
    || Set.isSubset inc ([ -1; -2; -3 ] |> Set.ofList)

let sum =
    List.sumBy (function
        | true -> 1
        | _ -> 0)

let solution1 = List.map isSafe >> sum
Expect.equal (data |> solution1) 2 "hot damn"

let puzzle = InputData.day2 ()
puzzle |> solution1 |> print 1

let solution2 =
    let slices (report: int list) =
        [ for i in 0 .. report.Length - 1 do
              (report[.. i - 1] @ report[i + 1 ..]) |> isSafe ]
        |> List.contains true

    List.map slices >> sum

Expect.equal (data |> solution2) 4 "hot damn"
puzzle |> solution2 |> print 2
