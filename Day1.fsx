#load "Data.fsx"
#load "Utils.fsx"
#r "nuget:Expecto"
open Expecto
open System
open Data
open Utils

let mutable print = (fun (result:int) -> printResult 1 1 result)
let left = [3;4;2;1;3;3]
let right = [4;3;5;3;9;3]

let count l x = l |> List.filter ((=)x) |> List.length

let countRight = count right

let folder s i = 
  s + ((countRight i) * i)

let actual = 
  left 
  |> List.fold folder 0

Expect.equal actual 31 "Something went wrong with folding"
