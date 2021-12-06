#load "Data.fsx"
#r "nuget:Expecto"
open System
open Expecto

let testData = ["0,9,5,9";"8,0,0,8";"9,4,3,4";"2,2,2,1";"7,0,7,4";"6,4,2,0";"0,9,2,9";"3,4,1,4";"0,0,8,8";"5,5,8,2";]
let split (c:char) (s:string) = s.Split(c)
let range a b = if a > b then [a .. -1 .. b] else [a .. b]

type Position = {
    X: int
    Y: int
}
type Segment = {
    From: Position
    To: Position
}
let isLinear s = s.From.X = s.To.X || s.From.Y = s.To.Y
let notLinear = isLinear >> not
let unfoldLinearSegment (s:Segment) =
    let xs = range s.From.X s.To.X
    let ys = range s.From.Y s.To.Y
    xs |> List.collect (fun x -> List.zip (List.init (List.length ys) (fun _ -> x)) ys)
let unfoldUnlinearSegment (s:Segment) =
    let xs = range s.From.X s.To.X
    let ys = range s.From.Y s.To.Y
    List.zip xs ys

let unfoldSegment (s:Segment) = 
    match isLinear s with
    | true -> unfoldLinearSegment s
    | false -> unfoldUnlinearSegment s

let Position x y = {X = x; Y = y}
let Segment x1 y1 x2 y2 = {From = Position x1 y1; To = Position x2 y2}

let parse (s:string) =
    let ps = s |> split (',') |> Array.map int
    Segment ps[0] ps[1] ps[2] ps[3]

let parseList = List.map parse

let processData = List.collect unfoldSegment >> List.countBy id >> List.filter (fun (_,x) -> x > 1) >> List.length

let testResult = parseList testData |> List.filter isLinear |> processData
let expectedResult = 5
Expect.equal testResult expectedResult "Test failed"

parseList (Data.ventData()) |> List.filter isLinear |> processData |> printfn "Results for day 5: %i"

let testResult2 = parseList testData |> processData
let expectedResult2 = 12
Expect.equal testResult2 expectedResult2

parseList (Data.ventData()) |> processData |> printfn "Results for day 5 part 2: %i"