let private readFile input = System.IO.File.ReadAllLines(input) |> List.ofArray
let private readTransformFile fn = readFile >> List.map fn

let sonarData       () = readTransformFile int "./Datafiles/day1.txt"
let movementData    () = readFile "./Datafiles/day2.txt"
let powerData       () = readFile "./Datafiles/day3.txt"
let bingoDrawData   () = readTransformFile int "./Datafiles/day4Draws.txt"
let bingoCardData   () = readFile "./Datafiles/day4Cards.txt"
let ventData        () = readFile "./Datafiles/day5.txt"