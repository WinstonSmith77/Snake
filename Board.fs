[<RequireQualifiedAccess>]
module Board

open System
open Microsoft.FSharp.Collections
open Basic
open Microsoft.FSharp.Core

let startMaxLength = 10
let boardWidth = Console.WindowWidth
let boardHeight = Console.WindowHeight
let increaseLenghtEverUpdate = 50UL

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
      Snake: Pos List }

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

let Create =
    [ { Mode =
            InGame
                { Current = { X = 3; Y = 3 }
                  Direction = Direction.Right
                  Snake = List.empty }
        Progress =
            { Score = 0
              MaxLength = startMaxLength
              Start = DateTime.Now
              TimeRunning = TimeSpan.Zero
              Ticks = 0UL } } ]

type ActionFromInput =
    | NewDirection of Direction
    | BossKeyPressed
    | Space

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

let private updateInGame inGame progress direction =
    let clip maxX x = (x + maxX) % maxX

    let newPosition =
        match direction with
        | Direction.Left ->
            { inGame.Current with
                  X = inGame.Current.X - 1 }
        | Direction.Right ->
            { inGame.Current with
                  X = inGame.Current.X + 1 }
        | Direction.Up ->
            { inGame.Current with
                  Y = inGame.Current.Y - 1 }
        | Direction.Down ->
            { inGame.Current with
                  Y = inGame.Current.Y + 1 }
        |> (fun pos ->
            { X = pos.X |> clip boardWidth
              Y = pos.Y |> clip boardHeight })

    let snakeBitesItSelf = List.contains newPosition inGame.Snake

    if snakeBitesItSelf then
        { Mode = GameOver { FrameForReplay = 0 }
          Progress = progress }
    else
        let newSnake =
            newPosition :: inGame.Snake
            |> (fun snake -> List.take (Math.Min(List.length snake, progress.MaxLength)) snake)

        let doIncreaseLength =
            progress.Ticks % increaseLenghtEverUpdate = 0UL

        let newMaxLength =
            progress.MaxLength
            + if doIncreaseLength then 1 else 0

        let newScore =
            progress.Score
            + if doIncreaseLength then 10 else 1

        { Mode =
              InGame
                  { Current = newPosition
                    Snake = newSnake
                    Direction = direction }
          Progress =
              { progress with
                    MaxLength = newMaxLength
                    Score = newScore
                    Ticks = progress.Ticks + 1UL
                    TimeRunning = (progress.Start - DateTime.Now) } }

let UpdateState states =
    let state = List.head states
    let { Mode = mode; Progress = progress } = state

    match mode with
    | GameOver gameOver ->
        match readActionFromInput () with
        | Some BossKeyPressed -> Engine.NewState.Exit
        | Some Space -> Engine.NewState.Result Create
        | Some _
        | None ->
            let history = List.tail states

            let newGameOver =
                { FrameForReplay = gameOver.FrameForReplay + 1 }

            let top =
                { Mode = GameOver newGameOver
                  Progress = progress }

            Engine.NewState.Result(top :: history)
    | InGame inGame ->
        let updateInGameBakedIn =
            (fun direction -> (updateInGame inGame progress direction) :: states)
            >> Engine.NewState.Result

        match readActionFromInput () with
        | Some BossKeyPressed -> Engine.NewState.Exit
        | Some (NewDirection newDirection) -> updateInGameBakedIn newDirection
        | _ -> updateInGameBakedIn inGame.Direction

let private output newFrameBufferWithProblems oldFrameBuffer =
    let newFrameBuffer =
        newFrameBufferWithProblems
        //no two pixels on one spot
        |> List.groupBy (fun pixel -> pixel.Pos)
        |> List.map (fun (_, pixels) -> List.last pixels)


    let newSet = Set.ofList newFrameBuffer
    let oldSet = Set.ofList oldFrameBuffer

    let toDraw = Set.difference newSet oldSet
    let toRemove = Set.difference oldSet newSet

    let draw remove pixel =
        Console.SetCursorPosition(pixel.Pos.X, pixel.Pos.Y)
        Console.ForegroundColor <- pixel.Color
        Console.Write(if remove then ' ' else pixel.Text)

    Set.iter (draw true) toRemove
    Set.iter (draw false) toDraw

    newFrameBuffer

let private createChar pos text =
    Seq.mapi
        (fun i c ->
            { Pos = { X = pos.X + i; Y = pos.Y }
              Text = c
              Color = ConsoleColor.White })
        text
    |> List.ofSeq

let private createInGame inGame progress =
    let createPixel i pos =
        { Pos = pos
          Text = '█'
          Color =
              if i % 2 = 0 then
                  ConsoleColor.DarkYellow
              else
                  ConsoleColor.Cyan }

    let snake = List.mapi createPixel inGame.Snake

    let score =
        (createChar { X = 1; Y = 1 } (progress.Score.ToString("D6")))

    let timeText = progress.TimeRunning.ToString(@"mm\:ss")

    let time =
        (createChar
            { X = boardWidth - timeText.Length - 1
              Y = 1 }
            timeText)

    let length =
        (createChar { X = 1; Y = boardHeight - 1 } (progress.MaxLength.ToString("000")))

    let frameBuffer =
        [ snake; score; time; length ] |> List.collect id

    frameBuffer

let private outputInGame inGame progress oldFrameBuffer =
    let frameBuffer = createInGame inGame progress
    output frameBuffer oldFrameBuffer

let private bigR =
    [ "XXXXXX   "
      "X      X "
      "X      X "
      "XXXXXX   "
      "X     X  "
      "X      X "
      "X      X " ]
    |> List.map (fun line -> line.Trim())

let private createCenterText (text: String) offsetY =
    let x = (boardWidth - text.Length) / 2
    (createChar { X = x; Y = boardHeight / 2 + offsetY } text)

let private putLogo pos logo =
    List.mapi (fun y -> createChar { X = pos.X; Y = pos.Y + y }) logo
    |> List.ofSeq
    |> List.collect id

let private outputGameOver gameOver states oldFrameBuffer =
    let text = "Game Over"
    let text2 = "Spacebar to continue"

    let showText =
        List.exists (fun p -> p.Text = text.Chars(0)) oldFrameBuffer
        |> not

    let texts =
        if showText then
            [ putLogo { X = 3; Y = 3 } bigR
              createCenterText text 0
              createCenterText text2 1 ]
        else
            [ putLogo { X = 3; Y = 3 } bigR ]

    let chooseInGames state =
        match state.Mode with
        | InGame inGame -> Some(inGame, state.Progress)
        | _ -> None

    let allInGames =
        List.choose chooseInGames states |> List.rev

    let countInGames = List.length allInGames

    let inGameToShow =
        List.item (gameOver.FrameForReplay % countInGames) allInGames

    let newBuffer =
        [ createInGame (fst inGameToShow) (snd inGameToShow)
          List.collect id texts ]

        |> List.collect id

    output newBuffer oldFrameBuffer

let Output states oldFrameBuffer =
    let state = List.head states
    let { Mode = mode; Progress = progress } = state

    match mode with
    | InGame inGame -> outputInGame inGame progress oldFrameBuffer
    | GameOver gameOver -> outputGameOver gameOver states oldFrameBuffer
