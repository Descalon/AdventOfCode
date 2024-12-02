#load "Data.fsx"
#load "Utils.fsx"
#r "nuget:Expecto"

open Expecto
open System
open Data
open Utils

let mutable print = (fun (result: int) -> printResult 2 2 result)

let data =
    [ [ 7; 6; 4; 2; 1 ]
      [ 1; 2; 7; 8; 9 ]
      [ 9; 7; 6; 2; 1 ]
      [ 1; 3; 2; 4; 5 ]
      [ 8; 6; 4; 4; 1 ]
      [ 1; 3; 6; 7; 9 ]
      [ 48; 46; 47; 49; 51; 54; 56 ]
      [ 1; 4; 3; 2; 1 ] ]

let check report =
  let rec fn depth report =
    if depth > 1 then false else
      let op = 
        match report with
        | [] -> failwith "nope"
        | _::[] -> failwith "nope"
        | x::y::_ -> match x-y |> sign with | -1 -> (<) | _ -> (>)

      let rec fn' acc prev = function
        | [] -> acc
        | _::[] -> acc
        | x::y::xs ->
          let predicate1 = 
            let inrange n = n >= 1 && n <= 3
            (x-y) |> abs |> inrange
          let predicate2 = op x y

          let acc' = acc && predicate1 && predicate2
          if acc' then fn' acc' (prev@[x])(y::xs)
          else
            let cont i = prev@[i]@xs
            [ cont x; cont y; report |> List.tail ]
            |> List.map (fn (depth+1))
            |> List.fold (fun s i -> s || i) false

      fn' true [] report
    
  fn 0 report


let actual = data |> List.map check
let expected = [ true; false; false; true; true; true; true; true ]

Expect.equal actual expected "'Cause your friends don't dance, and if you don't dance, then you ain't no friend of mine"

let solution = List.map check >> List.sumBy (fun x -> if x then 1 else 0)

Expect.equal (data |> solution) 6 "Whoopsy"

// InputData.day2 () |> solution |> print
