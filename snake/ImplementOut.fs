module ImplementOut

open System
open GameTypes

type ConsoleOut() =
    interface IDraw with
        member this.DrawPixel(remove) (pixel) =
              Console.SetCursorPosition(pixel.Pos.X, pixel.Pos.Y)
              Console.ForegroundColor <- pixel.Color
              Console.Write(if remove then ' ' else pixel.Text)        
