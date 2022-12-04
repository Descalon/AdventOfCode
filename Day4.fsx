#load "Data.fsx"
#load "Utils.fsx"
#r "nuget:Expecto"

open System
open Expecto

let print p = Utils.printResult 4 p
let testdata =[ 
    [ 2..4 ], [ 6..8 ]
    [ 2..3 ], [ 4..5 ]
    [ 5..7 ], [ 7..9 ]
    [ 2..8 ], [ 3..7 ]
    [ 6..6 ], [ 4..6 ]
    [ 2..6 ], [ 4..8 ] 
]

let transform = function | true -> Some true | _ -> None

let contains (p: 'a list) (q: 'a list) = 
    q |> List.forall (fun x -> List.contains x p) 

let isFullyContained (a,b) = (contains a b || contains b a) |> transform

let testResult = testdata |> List.choose isFullyContained |> Seq.length
let expectedResult = 2
Expect.equal testResult expectedResult "Test result is incorrect"

Data.elfpairData () |> List.choose isFullyContained |> Seq.length |> print 1

let containsAny ((p: 'a list),(q: 'a list)) = 
    q |> List.exists (fun x -> List.contains x p) |> transform

let testResult2 = testdata |> List.choose containsAny |> Seq.length
let expectedResult2 = 4
Expect.equal testResult2 expectedResult2 "Test result is incorrect"

Data.elfpairData () |> List.choose containsAny |> Seq.length |> print 2