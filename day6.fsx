#load "Data.fsx"
#r "nuget:Expecto"
open System
open Expecto


let parse ns = Seq.init 9 id |> Seq.mapi (fun _ i -> List.filter ((=) i) ns |> List.length |> int64)
let flip fn a b = fn b a 
let prepend a b = Seq.append b a

let step ns = 
    let head = Seq.head ns
    ns |> Seq.skip 1 |> prepend [head] |> Seq.mapi (fun i x -> if i <> 6 then x else x + head)

let loop n ns = [1..n] |> List.fold (fun s _ -> step s) ns

let loop80 = Data.lanternFishData () |> parse |> loop 80
loop80 |> Seq.sum |> printfn "Results day 6: %i"

let loop256 = loop80 |> loop (256 - 80)
loop256 |> Seq.sum |> printfn "Results day 6 part two: %i"