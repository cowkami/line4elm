module Game exposing (..)

import Board exposing (..)


type Result =
    Playing |
    Draw |
    BlackWin |
    WhiteWin


type alias Game =
    { turn : Stone
    , result : Result
    }


initGame : Game
initGame =
    { turn = Black
    , result = Playing}


nextStep : Board -> Game -> Game
nextStep board game =
    { turn = turnColor game.turn
    , result = checkResult board game}


turnColor : Stone -> Stone
turnColor color =
    case color of
        Black -> White
        White -> Black
        NoStone -> NoStone


checkResult : Board -> Game -> Result
checkResult board game =
