[<RequireQualifiedAccess>]
module Engine

open System
open System.Threading
open Microsoft.FSharp.Core

let frameTime = 100

type NewState<'a> =
    | Result of 'a
    | Exit

let Run updateState output startState =
    let callUpdateAndWait state frameBuffer =
        let start = DateTime.Now

        match updateState state with

        | Exit -> Exit
        | Result newState ->
            let toWait =
                frameTime
                - ((DateTime.Now - start).TotalMilliseconds
                   |> Math.Ceiling
                   |> int)

            if toWait > 0 then Thread.Sleep toWait

            let frameBuffer = output newState frameBuffer

            (newState, frameBuffer) |> Result

    let mutable newState = startState
    let mutable newFrameBuffer = List.Empty
    let mutable doBreak = false

    while doBreak |> not do
        match callUpdateAndWait newState newFrameBuffer with
        | Exit -> doBreak <- true
        | Result (state, frameBuffer) ->
            newState <- state
            newFrameBuffer <- frameBuffer
