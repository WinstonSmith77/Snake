module Program

open System

Console.CursorVisible <- false
Engine.Run GameLogic.UpdateState GameOutput.Output GameLogic.Create List.Empty |> ignore