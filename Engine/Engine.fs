[<RequireQualifiedAccess>]
module Engine

open System
open System.Threading
open Microsoft.FSharp.Core

let private frameTime = 100

let rec Run updateState output state frameBuffer =
    let start = DateTime.Now

    match updateState state with
    | Some newState ->
        let toWait =
            frameTime
            - ((DateTime.Now - start).TotalMilliseconds
               |> Math.Ceiling
               |> int)

        if toWait > 0 then Thread.Sleep toWait

        let frameBuffer = output newState frameBuffer
        Run updateState output newState frameBuffer
      | None -> None   