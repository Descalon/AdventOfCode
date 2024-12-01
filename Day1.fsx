#load "Data.fsx"
#load "Utils.fsx"
#r "nuget:Expecto"
open Expecto
open System
open Data
open Utils

let mutable print = (fun (result:int) -> printResult 1 2 result)
let left = [3;4;2;1;3;3]
let right = [4;3;5;3;9;3]

let solution (l,r) = 
  let count xs x = xs |> List.filter ((=)x) |> List.length

  let folder s i = 
    s + ((count r i) * i)

  l |> List.fold folder 0

let actual = solution (left,right)

Expect.equal actual 31 "Something went wrong with folding"

InputData.day1 () |> solution |> print
