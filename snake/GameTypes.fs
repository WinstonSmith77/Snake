module GameTypes

open System


let StartLength = 10
let BoardWidth = Console.WindowWidth
let BoardHeight = Console.WindowHeight
let NumberOfFoods = 200
let NumberOfRocks = 20

let FrameTime = 100

let ScoreStep = 1
let ScoreFood = 11 * ScoreStep

type Pos = { X: int; Y: int }

type Pixel =
    { Pos: Pos
      Text: char
      Color: ConsoleColor }


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
      Food: Pos List
      Rocks: Pos List
      Walls : Pos List}

type GameMode =
    | InGame of InGame
    | GameOver of GameOver

type Progress =
    { Score: int
      MaxLength: int
      Start: DateTime
      Ticks: uint64
      TimeRunning: TimeSpan }

type Game = { Mode: GameMode; Progress: Progress }

type ActionFromInput =
    | NewDirection of Direction
    | BossKeyPressed
    | Space

