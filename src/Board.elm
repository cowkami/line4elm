module Board exposing (..)

import Array exposing (Array, repeat)
import Debug
import Maybe exposing (andThen)


maxZ : Int
maxZ = 4

maxX : Int
maxX = 4

maxY : Int
maxY = 4

type Stone =
    Black |
    White |
    NoStone

type alias Column =
    Array Stone

type alias Base =
    Array Column

type alias Board =
    { base : Base
    , numStones : Array Int
    , updated : Bool
    }

isInRange : Int -> Int -> Int -> Bool
isInRange n min max =
    min <= n && n < max

corr2Dto1D : Int -> Int -> Maybe Int
corr2Dto1D x y =
    if isInRange x 0 maxX && isInRange y 0 maxY then
        Just (x + y * maxY)
    else
        Nothing

initBoard : Board
initBoard =
    { base = repeat (maxX * maxY) (repeat maxZ NoStone)
    , numStones = repeat (maxX * maxY) 0
    , updated = True 
    }

getArray2D : Int -> Int -> Array a -> Maybe a
getArray2D x y arr =
    corr2Dto1D x y |>
    andThen (\ corr -> Array.get corr arr)


getStoneColor : Int -> Int -> Int -> Board -> Maybe Stone
getStoneColor x y nColumn board =
    getArray2D x y board.base |>
    andThen (\ column -> Array.get nColumn column)


setStone : Int -> Int -> Stone -> Board -> Board
setStone x y stone board =
    let
        maybeCorr = corr2Dto1D x y
        maybeNCol = getArray2D x y board.numStones
        maybeColumn = getArray2D x y board.base
        updateBase = \ corr nCol column -> Array.set corr (Array.set nCol stone column) board.base
        updateNumStones = \ corr nCol -> Array.set corr (nCol + 1) board.numStones

        maybeBase = Maybe.map3 updateBase maybeCorr maybeNCol maybeColumn
        maybeNumStones = Maybe.map2 updateNumStones maybeCorr maybeNCol
    in
        Maybe.map2 (\ base numStones -> 
                {board | base = base
                , numStones = numStones
                , updated = True
                }) maybeBase maybeNumStones
            |> Maybe.withDefault {board | updated = False}