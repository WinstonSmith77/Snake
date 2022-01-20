module GameTypes

open System
open Basic

let StartLength = 10
let BoardWidth = Console.WindowWidth
let BoardHeight = Console.WindowHeight
let NumberOfFoods = 100

[<RequireQualifiedAccess>]
type Direction =
    | Left
    | Right
    | Up
    | Down

type GameOver = { FrameForReplay: int }

type InGame =
    { Current: Pos
      Direction: Direction
      Snake: Pos List
      Food: Pos List }

type Mode =
    | InGame of InGame
    | GameOver of GameOver

type Progress =
    { Score: int
      MaxLength: int
      Start: DateTime
      Ticks: uint64
      TimeRunning: TimeSpan }

type Board = { Mode: Mode; Progress: Progress }
