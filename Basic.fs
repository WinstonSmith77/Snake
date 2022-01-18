module Basic

type Pos = { X: int; Y: int }

type Pixel = { Pos: Pos; Text: char }

type GameState<'a> = { State: 'a }
