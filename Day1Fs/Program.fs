let readData (path) =
    System.IO.File.ReadLines path
    |> Seq.map(fun line ->
        match line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries) with
        | [|p1; p2|] ->
            int p1, int p2

        | _ -> failwith $"Could not interpret line {line}"
    )
    |> Seq.toArray
    |> Array.unzip

let leftList, rightList = readData "..//..//..//..//Day1Data.txt"

let lookupList = rightList |> Array.sort

let difference =
    leftList
    |> Array.sort
    |> Array.mapi(fun index x -> abs(x - lookupList[index]))
    |> Array.sum

printfn $"Sum : {difference}"

let duplicateCount = 
    rightList
    |> Array.groupBy id
    |> Array.map (fun (i, x) -> i, x.Length)
    |> Map.ofArray

let similarity =
    leftList
    |> Array.map(fun leftId -> 
        let count=
            duplicateCount.TryFind leftId
            |> Option.defaultValue 0
        leftId * count
    )
    |> Array.sum

printfn $"SimilarityScore : {similarity}"
