type Direction =
    | Right
    | Left
    | Up
    | Down

let testData =
    """....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."""
    |> fun x -> x.Split("\n") |> Array.collect(fun x -> x.Split("\r")) |> Array.filter (fun x -> x.Length > 0)

let xmasToIndex =
    function
    | '#' -> true, None
    | '.' -> false, None
    | '^' -> false, Some Up
    | '>' -> false, Some Right
    | 'v' -> false, Some Down
    | '<' -> false, Some Left
    | _ -> failwith "Illegal character"

let floorPlan, (startPosition, startDirection) =
    System.IO.File.ReadLines "..//Day6Data.txt"
    |> Seq.toArray
    |> Array.map (fun x -> x.ToCharArray() |> Array.map xmasToIndex |> Array.unzip)
    |> fun x ->
        
        let s =
            x
            |> Array.indexed
            |> Array.collect (fun (row, (_ ,y)) ->
                y
                |> Array.indexed
                |> Array.choose (fun (column, v) -> v |> Option.map (fun z -> (row, column), z))
            )
            |> Array.head
        
        let f = x |> Array.map fst
        
        f, s

let down (rowIndex, columnIndex ) = (rowIndex + 1, columnIndex)

let up (rowIndex, columnIndex ) = (rowIndex - 1, columnIndex)

let diagRightDown (rowIndex, columnIndex ) = (rowIndex + 1, columnIndex + 1)

let diagLeftUp (rowIndex, columnIndex ) = (rowIndex - 1, columnIndex - 1)

let diagRightUp (rowIndex, columnIndex ) = (rowIndex - 1, columnIndex + 1)

let diagLeftDown (rowIndex, columnIndex ) = (rowIndex + 1, columnIndex - 1)

let right (rowIndex, columnIndex ) = (rowIndex, columnIndex + 1)

let left (rowIndex, columnIndex ) = (rowIndex, columnIndex - 1)

let allDirections = [|down; up; diagRightDown; diagLeftUp; right; left; diagRightUp; diagLeftDown|]

let createMoveDirection (direction) =
    match direction with
    | Up -> up
    | Right -> right
    | Down -> down
    | Left -> left


let rec walk (currentDirection : Direction, moveOn : int * int -> int * int, currentPosition, trace : Set<int*int>)=
    
    let rowIndex, columnIndex = moveOn currentPosition

    let t = trace.Add currentPosition
    
    match floorPlan |> Array.tryItem rowIndex |> Option.bind (fun x -> x |> Array.tryItem columnIndex) with
    | None -> trace.Add (rowIndex, columnIndex)
    | Some false ->
        //walk on
        
        walk (currentDirection, moveOn, (rowIndex, columnIndex), t)
    
    | Some true ->
        //turn
        
        let newDirection =
            
            match currentDirection with
            | Up -> Right
            | Right -> Down
            | Down -> Left
            | Left -> Up
            
        walk (newDirection, createMoveDirection newDirection, currentPosition, t)

//part1
let route =
    walk (startDirection, createMoveDirection startDirection, startPosition, Set.empty )
    
let stepCount = route.Count
    
printfn $"step count: {stepCount}"

let rec checkLoop (obstacle : int * int, currentDirection : Direction, moveOn : int * int -> int * int, currentPosition, trace : Set<(int*int)*Direction>)=
    
    let rowIndex, columnIndex = moveOn currentPosition

    match floorPlan |> Array.tryItem rowIndex |> Option.bind (fun x -> x |> Array.tryItem columnIndex) with
    | None -> 0
    //cater for the new obstacle
    | Some false when obstacle <> (rowIndex, columnIndex) ->
        //walk on
        
        checkLoop (obstacle, currentDirection, moveOn, (rowIndex, columnIndex), trace)
    
    | _ ->
        
        //check if we are repeating the route
        match trace.Contains(currentPosition, currentDirection) with
        | true -> 1
        | false ->
            
            //turn
            
            let newDirection =
                
                match currentDirection with
                | Up -> Right
                | Right -> Down
                | Down -> Left
                | Left -> Up
                
            let t = trace.Add (currentPosition, currentDirection)
                
            checkLoop (obstacle, newDirection, createMoveDirection newDirection, currentPosition, t)
       
//part2
let obstaclePositions =
    floorPlan
    |> Array.mapi(fun rowIndex row -> 
        row
        |> Array.mapi (fun colIndex isObstacle -> 
            match not isObstacle && startPosition <> (rowIndex, colIndex) with
            | true ->
                
                let a = checkLoop ((rowIndex, colIndex), startDirection, createMoveDirection startDirection, startPosition, Set.empty)
                
                
                a
            | false -> 0
            
        )
        |> Array.sum
    )
    |> Array.sum
    
    
printfn $"obstacle position count: {obstaclePositions}"