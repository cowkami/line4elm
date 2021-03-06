module Models exposing (..)

import Angle exposing (Angle)
import Board exposing (Board)


type alias Model =
    { azimuth : Angle
    , elevation : Angle
    , orbiting : Bool
    , board : Board
    , selected : (Int, Int)
    , viewportSize : ViewportSize
    }

type alias ViewportSize =
    { width : Int, height : Int}