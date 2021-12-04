#load "Data.fsx"
#r "nuget:Expecto"
open System
open Expecto

type BingoNumber = Marked of int | Unmarked of int
type Card = Card of BingoNumber[,]
let split (c:char) (s:string) = s.Split(c)

module BingoNumber = 
    let isMarked = function | Marked _ -> true | Unmarked _ -> false
    let isUnmarked = isMarked >> not
    let flip = function | Marked x -> Unmarked x  | Unmarked x -> Marked x
    let UnmarkedMatch n = function | Unmarked x when x = n -> true | _ -> false
    let value = function | Marked x -> x | Unmarked x -> x

module Card =
    open BingoNumber
    let private flatten (arr: 'T[,]) = arr |> Seq.cast<'T> |> Seq.toArray
    let mark n (Card c) = c |> Array2D.map (fun x -> if UnmarkedMatch n x then flip x else x) |> Card
    let private isRowComplete rowIndex (Card c) = c[rowIndex,*] |> Array.forall isMarked
    let private isColComplete colIndex (Card c) = c[*,colIndex] |> Array.forall isMarked
    let private isRCComplete c idx= isRowComplete idx c || isColComplete idx c
    let isComplete (Card c) = [0 .. (Array2D.length1 c)-1] |> List.exists (isRCComplete (Card c))
    let score (Card c) = c |> flatten |> Array.filter isUnmarked |> Array.sumBy value


    let generateCard (data: string list) =
        let p = data |> List.map (split ',') |> Array.ofList
        Array2D.init 5 5 (fun i j -> p[i][j] |> int |> Unmarked) |> Card

let drawTestData = [7;4;9;5;11;17;23;2;0;14;21;24;10;16;13;6;15;25;12;22;18;20;8;19;3;26;1]

let cardTestData = ["22,13,17,11,0";"8,2,23,4,24";"21,9,14,16,7";"6,10,3,18,5";"1,12,20,15,19";"3,15,0,2,22";"9,18,13,17,5";"19,8,7,25,23";"20,11,10,24,4";"14,21,16,12,6";"14,21,17,24,4";"10,16,15,9,19";"18,8,23,26,20";"22,11,13,6,5";"2,0,12,3,7";]
let slice n xs = List.take n xs, List.skip n xs

let processData (data: string list) =
    let rec fn acc = function
    | [] -> acc
    | xs ->
        let (x,rest) = slice 5 xs
        fn (acc @ [Card.generateCard x]) rest
    fn [] data

let drawNumber n (cs: Card list) =
    let rec fn completed remaining= function
    | [] -> completed,remaining
    | x::xs ->
        let x' = Card.mark n x
        if Card.isComplete x' 
            then fn (Some (x',n)) remaining xs
            else fn completed (remaining @ [x']) xs
    fn None [] cs

let runBingo cs = 
    let rec fn completed remaining = function
    | [] -> completed
    | x::xs -> 
        let c',r' = drawNumber x remaining
        match c' with
        | None -> fn completed r' xs
        | Some c -> fn (completed @ [c]) r' xs
    fn [] cs

let cardCalculation (c,i) = Card.score c * i
let testCards = processData cardTestData
let run cards = runBingo cards >> List.head >> cardCalculation

let testResult = run testCards drawTestData
let expectedResults = 4512
Expect.equal testResult expectedResults "Test results not equal"

let cardData,drawData = (Data.bingoCardData() |> processData),Data.bingoDrawData()
printfn "Results of day 4: %i" (run cardData drawData)

let run' cards = runBingo cards >> List.last >> cardCalculation

let testResult2 = run' testCards drawTestData
let expectedResults2 = 1924
Expect.equal testResult2 expectedResults2 "Test results not equal"

printfn "Results of day 4 part 2: %i" (run' cardData drawData)