module Program

open System
open GameTypes

let private readActionFromInput () =
    let keyPressed =
        match Console.KeyAvailable with
        | false -> None
        | true -> Console.ReadKey(true).Key |> Some

    let someNewDirection = NewDirection >> Some

    match keyPressed with
    | None -> None
    | Some ConsoleKey.Escape -> Some BossKeyPressed
    | Some key ->
        match key with
        | ConsoleKey.LeftArrow -> Direction.Left |> someNewDirection
        | ConsoleKey.RightArrow -> Direction.Right |> someNewDirection
        | ConsoleKey.UpArrow -> Direction.Up |> someNewDirection
        | ConsoleKey.DownArrow -> Direction.Down |> someNewDirection
        | ConsoleKey.Spacebar -> Some Space
        | _ -> None
        
let draw remove pixel =
        Console.SetCursorPosition(pixel.Pos.X, pixel.Pos.Y)
        Console.ForegroundColor <- pixel.Color
        Console.Write(if remove then ' ' else pixel.Text)        

Console.CursorVisible <- false
Engine.Run (GameLogic.UpdateState readActionFromInput) (GameOutput.Output draw) GameLogic.Create List.Empty |> ignore