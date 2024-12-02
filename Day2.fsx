#load "Data.fsx"
#load "Utils.fsx"
#r "nuget:Expecto"
open Expecto
open System
open Data
open Utils

let mutable print = (fun (result:int) -> printResult 2 1 result)
let data =
    [ [ 7; 6; 4; 2; 1 ]
      [ 1; 2; 7; 8; 9 ]
      [ 9; 7; 6; 2; 1 ]
      [ 1; 3; 2; 4; 5 ]
      [ 8; 6; 4; 4; 1 ]
      [ 1; 3; 6; 7; 9 ] ]

let check report = 
  let getOp = function
  | [] -> failwith "damn"
  | _::[] -> failwith "damn"
  | x::y::_ -> match sign (x-y) with
                | -1 -> (<)
                | _ -> (>)


  let rec fn op acc = function
    | [] -> acc
    | _::[] -> acc
    | x::y::xs ->
      let p1 = 
        let inrange x = x >= 1 && x <= 3
        x-y |> abs |> inrange
      let p2 = op x y
      let acc' = acc && p1 && p2
      if not acc' then false else fn op acc' (y::xs)
  let op = getOp report

  fn op true report


let actual = data |> List.map check
let expected = [true;false;false;false;false;true]

Expect.equal actual expected "'Cause your friends don't dance, and if you don't dance, then you ain't no friend of mine";

let solution = List.map check >> List.sumBy (fun x -> if x then 1 else 0)

Expect.equal (data |> solution) 2 "Whoopsy"

InputData.day2 () |> List.map check |> List.sumBy (fun x -> if x then 1 else 0) |> print
