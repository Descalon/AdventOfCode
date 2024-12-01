open System

let private split (c: char) (input: string) =
    input.Split(c, StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

let rec fn left right =
    function
    | [] -> left, right
    | x :: xs ->
        let (l, r) =
            let s = x |> split ','
            int s[0], int s[1]

        let left' = left@[l]
        let right' = right@[r]
        fn left' right' xs

let testinput = [ "3,4"; "4,3"; "2,5"; "1,3"; "3,9"; "3,3" ]

fn [] [] testinput |> printfn "%A"
