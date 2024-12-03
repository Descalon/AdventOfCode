#load "Data.fsx"
#load "Utils.fsx"
#r "nuget:Expecto"

open Expecto
open System
open Data
open Utils
open System.Text.RegularExpressions

let print (result: int) = printResult 2 1 result

let printPassThru s o =
    printfn s o
    o

let data =
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+do()mul(8,5)don't()(mul(11,8)undon()?mul(8,5))"

let ignoreExpression = Regex("don't\(\).*?do\(\)")
let secondPass input = Regex("don't\(\).*").Replace(input, "")
let rex = Regex("mul\((\d*),(\d*)\)", RegexOptions.Compiled)

let solution input =
    let mul input =
        [ for x in rex.Matches(input) -> (int x.Groups[2].Value) * (int x.Groups[1].Value) ]

    let replaced = ignoreExpression.Replace(input, "")

    replaced |> secondPass |> mul |> List.sum

Expect.equal (solution data) 48 "Maybe I should have used this stupid argument :facepalm:"

InputData.day3 () |> solution |> print
