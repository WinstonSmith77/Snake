module GameOutput

open System
open GameTypes

let private removeStackedPixels pixels =
    pixels
    |> List.groupBy (fun pixel -> pixel.Pos)
    |> List.map (fun (_, pixels) -> List.last pixels)

let private output (draw: IDraw) newFrameBufferWithProblems oldFrameBuffer =
    let newFrameBuffer =
        newFrameBufferWithProblems |> removeStackedPixels

    let newSet = Set.ofList newFrameBuffer
    let oldSet = Set.ofList oldFrameBuffer

    let toDraw = Set.difference newSet oldSet
    let toRemove = Set.difference oldSet newSet


    Set.iter (draw.DrawPixel true) toRemove
    Set.iter (draw.DrawPixel false) toDraw

    newFrameBuffer

let private createText pos text =
    Seq.mapi
        (fun i c ->
            { Pos = { X = pos.X + i; Y = pos.Y }
              Text = c
              Color = ConsoleColor.White })
        text
    |> List.ofSeq

let private createInGameOutput inGame progress =

    let createItem c color pos = { Pos = pos; Text = c; Color = color }

    let snake =
        List.mapi
            (fun i ->
                createItem
                    '█'
                    (if i % 2 = 0 then
                         ConsoleColor.DarkYellow
                     else
                         ConsoleColor.Cyan))

            inGame.Snake

    let score =
        (createText { X = 2; Y = 2 } (progress.Score.ToString("D6")))

    let timeText = progress.TimeRunning.ToString(@"mm\:ss")

    let time =
        (createText
            { X = BoardWidth - timeText.Length - 2
              Y = 2 }
            timeText)

    let length =
        (createText { X = 2; Y = BoardHeight - 2 } (progress.MaxLength.ToString("000")))

    let food =
        List.map (createItem 'o' ConsoleColor.Green) inGame.Food

    let rocks =
        List.map (createItem '*' ConsoleColor.Yellow) inGame.Rocks

    let walls =
        List.map (createItem '█' ConsoleColor.Gray) inGame.Walls

    let frameBuffer =
        [ walls
          rocks
          food
          snake
          score
          time
          length ]
        |> List.collect id

    frameBuffer

let private outputInGame draw inGame progress oldFrameBuffer =
    let frameBuffer = createInGameOutput inGame progress
    output draw frameBuffer oldFrameBuffer

let private createCenterText (text: String) offsetY =
    let x = (BoardWidth - text.Length) / 2
    (createText { X = x; Y = BoardHeight / 2 + offsetY } text)

let private outputGameOver draw gameOver states oldFrameBuffer =
    let text = "Game Over"
    let text2 = "Spacebar to continue"

    let showText =
        List.exists (fun p -> p.Text = text.Chars(0)) oldFrameBuffer
        |> not

    let texts =
        if showText then
            [ createCenterText text 0
              createCenterText text2 1 ]
        else
            []

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
        [ createInGameOutput (fst inGameToShow) (snd inGameToShow)
          List.collect id texts ]

        |> List.collect id

    output draw newBuffer oldFrameBuffer

let Output draw states oldFrameBuffer =
    let state = List.head states
    let { Mode = mode; Progress = progress } = state

    match mode with
    | InGame inGame -> outputInGame draw inGame progress oldFrameBuffer
    | GameOver gameOver -> outputGameOver draw gameOver states oldFrameBuffer
