#load "Data.fsx"
#r "nuget:Expecto"
open System
open Expecto

let testData = ["forward 5";"down 5";"forward 8";"up 3";"down 8";"forward 2";]

let split (s:string) = 
    let split = s.Split(' ')
    (split[0], int split[1])

let parse = function
| "forward", x -> (x,0)
| "down", x -> (0,x)
| "up", x -> (0,-x)
| _,_ -> (0,0)

let splitParse = split >> parse
let multTuple (a,b) = a*b
let addTuple (a,b) (c,d) = a+c, b+d

let processData = List.map splitParse >> List.fold addTuple (0,0) >> multTuple

let testResult = processData testData
let expectedResult = 150
Expect.equal testResult expectedResult

printfn "Results for day 2: %i" (processData Data.movementData)

let folder (a,b,c) (d,e) = a+d, b+ (d * (c + e)), c + e
let processData' = List.map splitParse >> List.fold folder (0,0,0) >> (fun (a,b,_) -> a,b) >> multTuple

let testResult2 = processData' testData
let expectedResult2 = 900
Expect.equal testResult2 expectedResult2 "foo"

printfn "Results for day 2 part 2: %i"  (processData' Data.movementData)