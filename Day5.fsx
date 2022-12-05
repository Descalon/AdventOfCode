#load "Data.fsx"
#load "Utils.fsx"
#r "nuget:Expecto"

open System
open Expecto

type Stack = char list
type StackCollection = Stack list

let print p = Utils.printResult 5 p
let testdata: StackCollection =
    [ [ 'N'; 'Z' ]
      [ 'D'; 'C'; 'M' ]
      [ 'P' ] ]

let testinstructions =
    [ (1, 2, 1)
      (3, 1, 3)
      (2, 2, 1)
      (1, 1, 2) ]

let pop n (s: Stack) =
    let head = s |> List.take n |> List.rev
    let rest = s |> List.skip n
    head, rest

let push (a: Stack) (b: Stack) : Stack = List.concat [ a; b ]

let replace index value list =
    if index > (list |> List.length) then
        list
    else
        list
        |> List.removeAt index
        |> List.insertAt index value

let move n f t (s: StackCollection) : StackCollection = 
    let (head,rest) = s[f] |> pop n
    let n = s[t] |> push head
    s |> replace f rest |> replace t n 

let moveFromTriple (n,f,t) = move n (f-1) (t-1)

let run instructions data = 
    instructions |> List.fold (fun s i -> moveFromTriple i s) data

let read (s:StackCollection) = s |> List.map List.head |> Array.ofList |> String

let testResult = testdata |> run testinstructions |> read
let expectedResult = "CMZ"
Expect.equal testResult expectedResult "Test result is incorrect"

Data.stackData () |> run (Data.stackInstructions ()) |> read |> print 1