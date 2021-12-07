#load "Data.fsx"
#r "nuget:Expecto"
open System
open Expecto

let testData = [16;1;2;0;4;2;7;1;2;14]

let median xs = 
    let s = xs |> List.sort
    let pivot = (List.length s / 2)-1
    let l = if (List.length s) % 2 = 0 then [List.item pivot s; List.item (pivot+1) s] else [List.item (pivot+1) s]
    (List.sum l) / l.Length

let average xs = 
    let l = List.length xs
    let s = xs |> List.sum
    (xs |> List.sum) / l

let fuelNeeded target = List.fold (fun s x -> (+) s <| abs (x - target)) 0

let testTarget = median testData
let testResult = fuelNeeded testTarget testData
let expectedResult = 37
Expect.equal testResult expectedResult "Test failed"

let data = Data.crabData ();
let target = median data;
fuelNeeded target data |> printfn "Results day 7: %i"

let findsum n = (n*(n+1) / 2)
let fuelNeeded' target = List.fold (fun s x -> (+) s <| findsum (abs(x-target))) 0

let testTarget2 = average testData
let testResult2 = fuelNeeded' testTarget2 testData
let expectedResult2 = 168
//Expect.equal testResult2 expectedResult2 "Test failed"

let target2 = average data
fuelNeeded' target2 data |> printfn "Results day 7 part 2: %i"