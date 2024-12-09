let testData =
    """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"""
    |> fun x -> x.Split("\n")

let xmasToIndex =
    function
    | 'X' -> 0
    | 'M' -> 1
    | 'A' -> 2
    | 'S' -> 3
    | _ -> failwith "Illegal character"

let dataArray =
    System.IO.File.ReadLines "..//Day4Data.txt"
    |> Seq.toArray
    |> Array.map (fun x -> x.ToCharArray() |> Array.map xmasToIndex)


let down (rowIndex, columnIndex ) = (rowIndex + 1, columnIndex)

let up (rowIndex, columnIndex ) = (rowIndex - 1, columnIndex)

let diagRightDown (rowIndex, columnIndex ) = (rowIndex + 1, columnIndex + 1)

let diagLeftUp (rowIndex, columnIndex ) = (rowIndex - 1, columnIndex - 1)

let diagRightUp (rowIndex, columnIndex ) = (rowIndex - 1, columnIndex + 1)

let diagLeftDown (rowIndex, columnIndex ) = (rowIndex + 1, columnIndex - 1)

let right (rowIndex, columnIndex ) = (rowIndex, columnIndex + 1)

let left (rowIndex, columnIndex ) = (rowIndex, columnIndex - 1)

let allDirections = [|down; up; diagRightDown; diagLeftUp; right; left; diagRightUp; diagLeftDown|]


let rec checkXMAS (moveDirection : int * int -> int * int) (currentPosition) (nextValue) =
    
    match nextValue with
    | 4 -> 1
    | _ ->
    
        let (rowIndex, columnIndex ) = moveDirection currentPosition
        
        match dataArray |> Array.tryItem rowIndex |> Option.bind (fun x -> x |> Array.tryItem columnIndex) with
        | Some v when v = nextValue ->
            
            checkXMAS moveDirection (rowIndex, columnIndex) (nextValue + 1)
        
        | _ -> 0

//part1
let xmasCount =
    dataArray
    |> Array.mapi(fun rowIndex row -> 
        row
        |> Array.mapi (fun colIndex thisChar -> 
            match thisChar with
            | 0 ->
                allDirections
                |> Array.sumBy (fun moveDirection -> 
                    checkXMAS moveDirection (rowIndex, colIndex) 1
                )
            | _ -> 0
            
        )
        |> Array.sum
    )
    |> Array.sum
    
    
printfn $"XMAS count: {xmasCount}"


let checkMAS currentPosition =
    
    let tryCooridante (row, col) =
         dataArray |> Array.tryItem row |> Option.bind (fun x -> x |> Array.tryItem col)

    match currentPosition |> diagRightDown |> tryCooridante, currentPosition |> diagLeftUp |> tryCooridante with
    | Some 1, Some 3 | Some 3, Some 1 ->
        
        match currentPosition |> diagLeftDown |> tryCooridante, currentPosition |> diagRightUp |> tryCooridante with
        | Some 1, Some 3 | Some 3, Some 1 ->
            1

        | _ -> 0
    
    | _ -> 0

//part 2
let masCrossCount =
    dataArray
    |> Array.mapi(fun rowIndex row -> 
        row
        |> Array.mapi (fun colIndex thisChar -> 
            match thisChar with
            | 2 ->
                checkMAS (rowIndex, colIndex)
                
            | _ -> 0
            
        )
        |> Array.sum
    )
    |> Array.sum
    
    
printfn $"X-MAS count: {masCrossCount}"