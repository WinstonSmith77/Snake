[<RequireQualifiedAccess>]
module Random

open Basic
open System

let random = new Random()
let private shuffle list =
    Seq.sortBy (fun _ -> random.NextDouble()) list

let FillSeqWithRandom toFillUp lengthToFillUpTo all =
    let lengthToAdd = lengthToFillUpTo - Seq.length toFillUp

    if (lengthToAdd > 0) then
        let toAdd = all |> shuffle |> Seq.take lengthToAdd
        Seq.append toFillUp toAdd
    else
        toFillUp
