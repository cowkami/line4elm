module Models exposing (..)

import Angle exposing (Angle)
import Board exposing (Board)


type alias Model =
    { azimuth : Angle
    , elevation : Angle
    , orbiting : Bool
    , board : Board
    }
