#load "Data.fsx"
#load "Utils.fsx"
#r "nuget:Expecto"
open System
open Expecto

let testData = [[1000;2000;3000];[4000];[5000;6000];[7000;8000;9000];[10000];]

let findMax (data: int list list) = 
    data |>
    List.map List.sum
    |> List.max

let testResult = findMax testData
let expectedResult = 24000
Expect.equal testResult expectedResult "Test result is incorrect"

let printDay1 part (input:int) = Utils.printResult 1 part input
//Data.calorieData() |> findMax |> printDay1 1

let findMax3 (data: int list list) = 
    data
    |> List.map List.sum
    |> List.sortDescending
    |> List.take 3
    |> List.sum

let testResult2 = findMax3 testData
let expectedResult2 = 45000
Expect.equal testResult2 expectedResult2 "Test result is incorrect"

Data.calorieData() |> findMax3 |> printDay1 2