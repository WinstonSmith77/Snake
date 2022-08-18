[<RequireQualifiedAccess>]
module Engine

open System
open System.Threading
open Microsoft.FSharp.Core
open GameTypes

let rec Run updateState output state frameBuffer =
    let start = DateTime.Now

    match updateState state with
    | Some newState ->
        let toWait =
            FrameTime
            - ((DateTime.Now - start).TotalMilliseconds
               |> Math.Ceiling
               |> int)

        if toWait > 0 then Thread.Sleep toWait

        let frameBuffer = output newState frameBuffer
        Run updateState output newState frameBuffer
      | None -> None   