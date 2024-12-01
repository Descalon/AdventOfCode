// #load "Data.fsx"
#r "nuget:Expecto"
open Expecto
open System

let testList1 = [3;4;2;1;3;3]
let testList2 = [4;3;5;3;9;3]

let s1 = List.sort testList1
let s2 = List.sort testList2
let absDiff (a:int,b:int) = Math.Abs (a-b)

let s = List.zip s1 s2 |> List.map (absDiff)

let expectedList = [2;1;0;1;2;5]
Expect.equal s expectedList "Zip error somewhere"
