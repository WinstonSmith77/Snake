module Program

open System

Console.CursorVisible <- false
Engine.Run Game.UpdateState Game.Output Game.Create 