
type Grid = Map<(int*int),int>
let getSizeX (g:Grid) = g |> Map

let addOne (g:Grid) : Grid =
    g |> Map.map (fun _ e -> e + 1)

let getNeighbours (x,y) (g:Grid) = 
    
    let neighbourIdx = [
        (-1,-1);(0,-1);(1,-1);
        (-1, 0);       (1, 0);
        (-1, 1);(0, 1);(1, 1);
    ]
    
