#load "Data.fsx"
#load "Utils.fsx"
#r "nuget:Expecto"
open Expecto
open System
open Data
open Utils

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
      // printfn "show me potato salad: %i-%i = %b" x y p1
      if not acc' then false else fn op acc' (y::xs)
  let op = getOp report

  fn op true report

let actual = data |> List.map check
let expected = [true;false;false;false;false;true]

Expect.equal actual expected "'Cause your friends don't dance, and if you don't dance, then you ain't no friend of mine";


