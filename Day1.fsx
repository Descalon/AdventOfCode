#load "Data.fsx"
#load "Utils.fsx"
#r "nuget:Expecto"
open Expecto
open System
open Data
open Utils

let mutable print = (fun (result:int) -> printResult 1 1 result)
let testList1 = [3;4;2;1;3;3]
let testList2 = [4;3;5;3;9;3]

let s1 = List.sort testList1
let s2 = List.sort testList2
let absDiff (a:int,b:int) = Math.Abs (a-b)

let solution (l,r) = List.zip l r |> List.map (absDiff)

let s = solution (s1,s2)

let expectedList = [2;1;0;1;2;5]
Expect.equal s expectedList "Zip error somewhere"

InputData.day1 () |> solution |> print
