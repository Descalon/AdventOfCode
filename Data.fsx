open System

let private split (c: char) (input: string) =
    input.Split(c, StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

let private replace (a: char) (b: char) (input: string) = input.Replace(a, b)
let private replaceSplit a b = replace a b >> split b

let private readFile input =
    System.IO.File.ReadAllLines(input) |> List.ofArray

let private readTransformFile fn = readFile >> List.map fn

[<RequireQualifiedAccess>]
module InputData =
  let day1() =
    let rec fn left right = function
      | [] -> left,right
      | x::xs ->
        let (l,r) = 
          let s = x |> split ','
          int s[0], int s[1]
        let left' = l::left
        let right' = r::right
        fn left' right' xs
    fn [] [] (readFile "datafiles/day1.txt")

