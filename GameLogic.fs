[<RequireQualifiedAccess>]
module GameLogic

open System
open Microsoft.FSharp.Collections
open Basic
open Microsoft.FSharp.Core
open GameTypes
open GameOutput

let private createWalls =
    let border =
        [ for x = 0 to BoardWidth - 1 do
            yield { X = x; Y = 0 }
            yield { X = x; Y = BoardHeight - 1 }
          for y = 0 to BoardHeight - 1 do
              yield { X = 0; Y = y }
              yield { X = BoardWidth - 1; Y = y } ]
        |> List.distinct

    let centerX = BoardWidth / 2
    let centerY = BoardHeight / 2

    let windows =
        [ for x = centerX - 5 to centerX + 5 do
            yield { X = x; Y = 0 }
            yield { X = x; Y = BoardHeight - 1 }
          for y = centerY - 5 to centerY + 5 do
              yield { X = 0; Y = y }
              yield { X = BoardWidth - 1; Y = y } ]

    [ border; windows ]
    |> List.collect id
    |> List.groupBy id
    |> List.collect
        (fun (pos, list) ->
            if List.length list % 2 = 1 then
                [ pos ]
            else
                [])


let Create =
    [ { Mode =
            InGame
                { Current = { X = 3; Y = 3 }
                  Direction = Direction.Right
                  Snake = List.Empty
                  Food = List.Empty
                  Rocks = List.Empty
                  Walls = createWalls }
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

let private fillUpCells otherCells numberOfCells oldCells =

    let cellsOccupied =
        otherCells |> List.collect id |> Set.ofList

    let cellsAvailable = Set.difference allCells cellsOccupied

    Random.FillSeqWithRandom oldCells numberOfCells cellsAvailable
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

    let createGameOver () =
        { Mode = GameOver { FrameForReplay = 0 }
          Progress =
              { progress with
                    Ticks = progress.Ticks + 1UL } }

    if snakeBitesItSelf then
        createGameOver ()
    else
        let snakeHasEatenFood = List.contains newPosition inGame.Food
        let snakeHasEatenRock = List.contains newPosition inGame.Rocks
        let snakeHasCrashedWall = List.contains newPosition inGame.Walls

        let newNewPosition =
            if snakeHasCrashedWall then
                inGame.Current
            else
                newPosition

        let newMaxLength =
            progress.MaxLength
            + if snakeHasEatenFood then 1 else 0
            + if snakeHasEatenRock then -1 else 0

        if newMaxLength = 0 then
            createGameOver ()
        else
            let newSnake =
                if snakeHasCrashedWall then
                    inGame.Snake
                else
                    newPosition :: inGame.Snake
                    |> List.truncate newMaxLength

            let newScore =
                progress.Score
                + match (snakeHasCrashedWall, snakeHasEatenFood) with
                  | true, _ -> 0
                  | false, true -> ScoreFood
                  | false, false -> ScoreStep

            let newFood =
                (if snakeHasEatenFood then
                     List.filter (fun pos -> pos <> newPosition) inGame.Food
                 else
                     inGame.Food)
                |> fillUpCells
                    [ inGame.Snake
                      inGame.Rocks
                      inGame.Walls ]
                    NumberOfFoods

            let newRocks =
                (if snakeHasEatenRock then
                     List.filter (fun pos -> pos <> newPosition) inGame.Rocks
                 else
                     inGame.Rocks)
                |> fillUpCells
                    [ inGame.Snake
                      inGame.Food
                      inGame.Walls ]
                    NumberOfRocks

            { Mode =
                  InGame
                      { Current = newNewPosition
                        Snake = newSnake
                        Direction = direction
                        Food = newFood
                        Rocks = newRocks
                        Walls = inGame.Walls }
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
        | Some BossKeyPressed -> None
        | Some Space -> Some Create
        | Some _
        | None ->
            let history = List.tail states

            let newGameOver =
                { FrameForReplay = gameOver.FrameForReplay + 1 }

            let newState =
                { Mode = GameOver newGameOver
                  Progress = progress }

            Some(newState :: history)
    | InGame inGame ->
        let updateInGameBakedIn =
            (fun direction -> (updateInGame inGame progress direction) :: states)
            >> Some

        match readActionFromInput () with
        | Some BossKeyPressed -> None
        | Some (NewDirection newDirection) -> updateInGameBakedIn newDirection
        | _ -> updateInGameBakedIn inGame.Direction
