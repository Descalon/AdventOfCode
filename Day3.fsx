#load "Data.fsx"
#load "Utils.fsx"
#r "nuget:Expecto"
open System
open Expecto

let testdata = ["vJrwpWtwJgWrhcsFMMfFFhFp";"jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL";"PmmdzqPrVvPwwTWBwg";"wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn";"ttgJtRGJQctTZtZT";"CrZsJsPPZsGzwwsLwLmpwMDw";]
let print p = Utils.printResult 2 p

let lookup = 
    let l = Seq.zip ['a' .. 'z'] (Seq.initInfinite ((+) 1))
    let u = Seq.zip ['A' .. 'Z'] (Seq.initInfinite ((+) 27))
    Seq.concat [l;u] |> Map.ofSeq

let get c = lookup[c]

let splitsack (input:string) =
    let l = (String.length input) / 2
    [input.Substring(0,l);input.Substring(l)]

let intersect input = input |> Seq.map Set.ofSeq |> Set.intersectMany |> Seq.head
let findCommonInSingle = splitsack >>  intersect

let sum = List.map (findCommonInSingle >> get) >> List.sum

let testResult = sum testdata
let expectedResult = 157
Expect.equal testResult expectedResult "Test result is incorrect"

// Data.sackData() |> sum |> print 1

// let splitList: 'a list -> 'b list list = 
//     let rec fn acc = function
//     | [] -> acc
//     | xs ->
//         let (n,ns) = xs |> List.splitAt 3
//         fn (acc @ [n]) ns
//     fn [] input
