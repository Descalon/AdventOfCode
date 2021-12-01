#load "Data.fsx"
#r "nuget:Expecto"
open System
open Expecto

let testData = [199; 200; 208; 210; 200; 207; 240; 269; 260; 263;]

let countIncreases = List.fold(fun (acc,q) p -> if p > q then (acc+1,p) else (acc,p) ) (0,Int32.MaxValue) >> fst

let testResult = countIncreases testData
let expectedResult = 7
Expect.equal testResult expectedResult "Test result is incorrect"

let result = countIncreases Data.sonarData
printfn "Result for day 1 = %i" result

let sumIff3 = function x::y::z::_ -> [x+y+z] | _ -> []

let slidingWindow (xs: int list) = 
    let rec fn acc = function [] -> acc | x::xs -> fn <| acc @ (sumIff3 (x::xs)) <| xs
    fn [] xs

let countIncreases' = slidingWindow >> countIncreases

let testResult2 = countIncreases' testData
let expectedResult2 = 5
Expect.equal testResult2 expectedResult2 "Test result 2 is incorrect"

let result2 = countIncreases' Data.sonarData
printfn "Result for day 1 part 2 = %i" result2

List.mapi