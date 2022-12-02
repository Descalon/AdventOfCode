#load "Data.fsx"
#load "Utils.fsx"
#r "nuget:Expecto"
open System
open Expecto

let testData = [[1000;2000;3000];[4000];[5000;6000];[7000;8000;9000];[10000];]

let processData (data: int list list) = 
    data |>
    List.map List.sum
    |> List.max

let testResult = processData testData
let expectedResult = 24000
Expect.equal testResult expectedResult "Test result is incorrect"

let printDay1 part (input:int) = Utils.printResult 1 part input
Data.calorieData() |> processData |> printDay1 1