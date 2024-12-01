open System

let private split (c: char) (input: string) =
    input.Split(c, StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

let private replace (a: char) (b: char) (input: string) = input.Replace(a, b)
let private replaceSplit a b = replace a b >> split b

let private readFile input =
    System.IO.File.ReadAllLines(input) |> List.ofArray

let private readTransformFile fn = readFile >> List.map fn

let caloriesFolder (l: int list list, c: int list) (input: string) =
    if (String.IsNullOrWhiteSpace input) then
        (l @ [ c ]), []
    else
        l, ([ int input ] @ c)

let private toRange =
    function
    | [ a; b; c; d ] -> [ a..b ], [ c..d ]
    | _ -> [], []

let private pairTransformer = replaceSplit ',' '-' >> List.map int >> toRange

let private toPair input =
    let x = input |> Seq.take 2
    Seq.item 0 x, Seq.item 1 x

