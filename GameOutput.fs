module GameOutput

open System
open Basic
open GameTypes

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

let private createInGameOutput inGame progress =
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
            { X = BoardWidth - timeText.Length - 1
              Y = 1 }
            timeText)

    let length =
        (createChar { X = 1; Y = BoardHeight - 1 } (progress.MaxLength.ToString("000")))

    let createFood pos =
        { Pos = pos
          Text = 'o'
          Color = ConsoleColor.Green}

    let food = List.map createFood inGame.Food

    let frameBuffer =
        [ food; snake; score; time; length; ] |> List.collect id

    frameBuffer

let private outputInGame inGame progress oldFrameBuffer =
    let frameBuffer = createInGameOutput inGame progress
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
    let x = (BoardWidth - text.Length) / 2
    (createChar { X = x; Y = BoardHeight / 2 + offsetY } text)

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
        |  InGame inGame -> Some(inGame, state.Progress)
        | _ -> None

    let allInGames =
        List.choose chooseInGames states |> List.rev

    let countInGames = List.length allInGames

    let inGameToShow =
        List.item (gameOver.FrameForReplay % countInGames) allInGames

    let newBuffer =
        [ createInGameOutput (fst inGameToShow) (snd inGameToShow)
          List.collect id texts ]

        |> List.collect id

    output newBuffer oldFrameBuffer

let Output states oldFrameBuffer =
    let state = List.head states
    let { Mode = mode; Progress = progress } = state

    match mode with
    | InGame inGame -> outputInGame inGame progress oldFrameBuffer
    | GameOver gameOver -> outputGameOver gameOver states oldFrameBuffer