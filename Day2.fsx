#load "Data.fsx"
#load "Utils.fsx"
#r "nuget:Expecto"
open Expecto
open System
open Data
open Utils

let mutable print = (fun (result:int) -> printResult 2 2 result)
let data =
    [ [ 7; 6; 4; 2; 1 ]
      [ 1; 2; 7; 8; 9 ]
      [ 9; 7; 6; 2; 1 ]
      [ 1; 3; 2; 4; 5 ]
      [ 8; 6; 4; 4; 1 ]
      [ 1; 3; 6; 7; 9 ] 
      [ 8; 9; 7; 6; 5 ]
      ]

let check report = 
  let rec retry o acc = function
    | [] -> acc
    | _::[] -> acc
    | x::y::xs ->
      let op = match o with | Some fn -> fn | None -> match sign (x-y) with | -1 -> (<) | _ -> (>) 
      let p1 = 
        let inrange x = x >= 1 && x <= 3
        x-y |> abs |> inrange
      let p2 = op x y
      let acc' = acc && p1 && p2
      if acc' then retry (Some op) acc' (y::xs) else false

  let rec fn o acc prev = function
    | [] -> acc
    | _::[] -> acc
    | x::y::xs ->
      let op = match o with | Some fn -> fn | None -> match sign (x-y) with | -1 -> (<) | _ -> (>) 
      let p1 = 
        let inrange x = x >= 1 && x <= 3
        x-y |> abs |> inrange
      let p2 = op x y
      let acc' = acc && p1
      let acc'' = acc' && p2
      if acc'' then 
        fn (Some op) acc'' (prev@[x]) (y::xs) 
      else 
        (retry None acc (prev@[x]@xs)) || (retry None acc (prev@[y]@xs))

  fn None true [] report


let actual = data |> List.map check
let expected = [true;false;false;true;true;true;true]

Expect.equal actual expected "'Cause your friends don't dance, and if you don't dance, then you ain't no friend of mine";

let solution = List.map check >> List.sumBy (fun x -> if x then 1 else 0)

Expect.equal (data |> solution) 5 "Whoopsy"

InputData.day2 () |> List.map check |> List.sumBy (fun x -> if x then 1 else 0) |> print
