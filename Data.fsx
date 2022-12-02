let private readFile input = System.IO.File.ReadAllLines(input) |> List.ofArray
let private readTransformFile fn = readFile >> List.map fn
