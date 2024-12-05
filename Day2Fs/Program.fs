// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"


let testData =
    """xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"""
    |> fun x -> x.Split("\n")

let readData : seq<string> -> string[] =
    Seq.collect (fun line ->
        line.Split([|"mul("|], System.StringSplitOptions.RemoveEmptyEntries)
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
    |> Seq.collect (fun line ->
        line.Split([|"mul("|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.filter(fun x ->
            match x.Split([|","; ")"|], System.StringSplitOptions.RemoveEmptyEntries) with
            | a when a.Length >= 2 ->
                match a[0].Length > 1 a[0].Length <= 3 && System.Int32.TryParse a[0] with
                | true, Some n ->
                )
    )
    |> Seq.toArray
    
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