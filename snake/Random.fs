[<RequireQualifiedAccess>]
module Random

open System

let random = Random()
let private shuffle list =
    Seq.sortBy (fun _ -> random.NextDouble()) list

let FillSeqWithRandom toFillUp lengthToFillUpTo allItems =
    let lengthToAdd = lengthToFillUpTo - Seq.length toFillUp

    let toAdd = allItems |> shuffle |> Seq.truncate lengthToAdd
    Seq.append toFillUp toAdd
