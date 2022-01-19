module Basic

open System

type Pos = { X: int; Y: int }

type Pixel =
    { Pos: Pos
      Text: char
      Color: ConsoleColor }

type GameState<'a> = { State: 'a }
