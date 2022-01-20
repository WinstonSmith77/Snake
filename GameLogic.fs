[<RequireQualifiedAccess>]
module GameLogic

open System
open Microsoft.FSharp.Collections
open Basic
open Microsoft.FSharp.Core
open GameTypes

let Create =
    [ { Mode =
            InGame
                { Current = { X = 3; Y = 3 }
                  Direction = Direction.Right
                  Snake = List.Empty
                  Food = List.Empty }
        Progress =
            { Score = 0
              MaxLength = StartLength
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

let private allCells =
    [ for x = 0 to BoardWidth - 1 do
          for y = 0 to BoardHeight - 1 do
              { X = x; Y = y } ]
    |> Set.ofList

let private fillUpFood snake oldFood =
    let cellsAvailable =
        (Set.difference allCells (Set.union (oldFood |> Set.ofList) (snake |> Set.ofList)))

    Random.FillSeqWithRandom oldFood NumberOfFoods cellsAvailable
    |> List.ofSeq

let private updateInGame inGame progress direction =
    let clip maxX x = (x + maxX) % maxX

    let shiftFromCurrent x y =
        { X = inGame.Current.X + x
          Y = inGame.Current.Y + y }

    let newPosition =
        match direction with
        | Direction.Left -> shiftFromCurrent -1 0
        | Direction.Right -> shiftFromCurrent 1 0
        | Direction.Down -> shiftFromCurrent 0 1
        | Direction.Up -> shiftFromCurrent 0 -1
        |> (fun pos ->
            { X = pos.X |> clip BoardWidth
              Y = pos.Y |> clip BoardHeight })

    let snakeBitesItSelf = List.contains newPosition inGame.Snake

    if snakeBitesItSelf then
        { Mode = GameOver { FrameForReplay = 0 }
          Progress = progress }
    else
        let snakeHasEaten = List.contains newPosition inGame.Food

        let newMaxLength =
            progress.MaxLength
            + if snakeHasEaten then 1 else 0

        let newSnake =
            newPosition :: inGame.Snake
            |> List.truncate newMaxLength

        let newScore =
            progress.Score + if snakeHasEaten then 10 else 1

        let newFood =
            (if snakeHasEaten then
                 List.filter (fun pos -> pos <> newPosition) inGame.Food
             else
                 inGame.Food)
            |> fillUpFood inGame.Snake

        { Mode =
              InGame
                  { Current = newPosition
                    Snake = newSnake
                    Direction = direction
                    Food = newFood  }
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
