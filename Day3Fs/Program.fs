let testData =
    """xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"""
    |> fun x -> x.Split("\n")

let checkNumber (a : string) =
    let tryInt = 
        match System.Int32.TryParse a with
        | true, a -> Some a
        | _ -> None

    1 <= a.Length && a.Length <= 3, tryInt


let processText (a : string) =
    a.Split([|"mul("|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map(fun x ->
        match x.Split([|","; ")"|], System.StringSplitOptions.RemoveEmptyEntries) with
        | a when a.Length >= 2 ->
            match checkNumber a[0], checkNumber a[1] with
            | (true, Some n1), (true, Some n2) ->
                n1 * n2
            | _ -> 0

        | _ -> 0
    )
    |> Array.sum

//part1
let mulSum =
    System.IO.File.ReadLines "..//Day3Data.txt"
    |> Seq.sumBy (fun line ->
        processText line
    )
    
printfn $"Multiply sum: {mulSum}"

//part2

let mulSum2 =
    System.IO.File.ReadLines "..//Day3Data.txt"
    |> String.concat ""
    |> fun x -> 
        let dontTree = x.Split([|"don't()"|], System.StringSplitOptions.RemoveEmptyEntries)

        let firstBit =
            dontTree[0]
            |> processText

        let theRest =
            dontTree
            |> Array.tail
            |> Array.sumBy(fun x ->
                x.Split([|"do()"|], System.StringSplitOptions.RemoveEmptyEntries)
                //throw away the don't part
                |> Array.tail
                |> Array.sumBy processText
            )

        firstBit + theRest
    
printfn $"Multiply sum: {mulSum2}"
