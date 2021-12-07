#load "Data.fsx"
#r "nuget:Expecto"
open System
open Expecto

let testData = [16;1;2;0;4;2;7;1;2;14]

let data = Data.crabData ();
let findMinimumFuel (f: int -> int) (xs: int list) = 
    seq { for k in [List.min xs .. List.max xs] -> (List.map ((-)k >> abs >> f) >> List.sum) xs} |> Seq.min

let sumById = findMinimumFuel id;
Expect.equal (sumById testData) 37 "Id test failed"
sumById data |> printfn "Day 7 part 1: %i" 

let closedFormSummation = findMinimumFuel (fun n -> n*(n+1) / 2)
Expect.equal (closedFormSummation testData) 168 "Summation test failed"
closedFormSummation data |> printfn "Day 7 part 2: %i"