open System

let private readFile input = System.IO.File.ReadAllLines(input) |> List.ofArray
let private readTransformFile fn = readFile >> List.map fn

let folder (l: int list list, c: int list) (input: string) =
    if (String.IsNullOrWhiteSpace input) then
        (l @ [c]), []
    else
        l, ([int input] @ c)

let calorieData () = "datafiles/calories.txt" |> readFile |> List.fold folder ([],[]) |> fst