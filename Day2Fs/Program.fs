// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"


let testData =
    """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"""
    |> fun x -> x.Split("\n")


let readData : seq<string> -> int[][] =
    Seq.map (fun line ->
        line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
    )
    >> Seq.toArray

let checkSafe (levels : int[]) =
    
    let diffs: int array = 
        levels
        |> Array.pairwise
        |> Array.map (fun (f, s) -> s - f)
    
    //check if the signs are all the same
    let checkIncreasing: bool =
        diffs
        |> Array.map(fun d -> 1 <= d && d <= 3)
        |> Array.reduce (&&)

    //check if the signs are all the same
    let checkDecreasing: bool =
        diffs
        |> Array.map(fun d -> -3 <= d && d <= -1)
        |> Array.reduce (&&)

    checkIncreasing || checkDecreasing

//part1
let safeCount =
    System.IO.File.ReadLines "..//Day2Data.txt"
    |> readData
    |> Array.sumBy(fun x ->

        let safe = checkSafe x

        match safe with
        | true -> 1
        | false -> 0

    )

printfn $"Safe Count : {safeCount}"

//part2

let safeCount2 =
    System.IO.File.ReadLines "..//Day2Data.txt"
    |> readData
    |> Array.sumBy(fun levels ->

        match checkSafe levels with
        | true -> 1
        | false -> 

            let safe = 
                (fun i  -> 
                    levels 
                    |> Array.removeAt i
                    |> checkSafe
                
                )
                |> Array.init levels.Length
                |> Array.reduce (||)

            match safe with
            | true -> 1
            | false -> 0

    )

printfn $"Safe Count when also removing levels : {safeCount2}"