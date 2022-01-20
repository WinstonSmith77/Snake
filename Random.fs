[<RequireQualifiedAccess>]
module Random

open Basic
open System

let random = new Random()
let private shuffle list =
    Seq.sortBy (fun _ -> random.NextDouble()) list

let FillSeqWithRandom toFillUp lengthToFillUpTo allItems =
    let lengthToAdd = lengthToFillUpTo - Seq.length toFillUp

    if (lengthToAdd > 0) then
        let toAdd = allItems |> shuffle |> Seq.truncate lengthToAdd
        Seq.append toFillUp toAdd
    else
        toFillUp
