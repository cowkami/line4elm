module Models exposing (..)

import Angle exposing (Angle)
import Board exposing (Board)
import Game exposing (Game)


type alias Model =
    { azimuth : Angle
    , elevation : Angle
    , orbiting : Bool
    , board : Board
    , game : Game
    , selected : (Int, Int)
    , viewportSize : ViewportSize
    }

type alias ViewportSize =
    { width : Int, height : Int}