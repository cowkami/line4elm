module Main exposing (..)


import Angle exposing (Angle)
import Browser
import Browser.Events
import Color
import Html exposing (Html)
import Html.Events
import Html exposing (div, text)
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import Scene3d
import Scene3d.Light as Light exposing (Light)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)

import Models exposing (Model)
import Objects exposing (..)
import Board exposing (Board)


type Msg
    = MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )
        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        MouseMove dx dy ->
            if model.orbiting then
                let 
                    rotationRate =
                        Angle.degrees 0.25 |> Quantity.per Pixels.pixel
                    
                    rotation numPixels =
                        numPixels |> Quantity.at rotationRate
                    
                    newAzimuth =
                        model.azimuth |> Quantity.minus (rotation dx)
                    
                    newElevation =
                        model.elevation
                            |> Quantity.plus (rotation dy)
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                in
                ( { model | azimuth = newAzimuth, elevation = newElevation }
                , Cmd.none )
            else
                ( model, Cmd.none )


init : () -> ( Model, Cmd Msg )
init () =
    ( { azimuth = Angle.degrees 45
      , elevation = Angle.degrees 40
      , orbiting = False
      , board = Board.initBoard
      }
    , Cmd.none
    )

view : Model -> Html Msg
view model =
    div []
        [ div [ Html.Events.onMouseDown MouseDown ]
            [ Scene3d.custom
                { lights = Scene3d.twoLights lightBulb overheadLighting
                , camera = camera model
                , clipDepth = Length.meters 0.1
                , dimensions = ( Pixels.int 1200, Pixels.int 900 )
                , antialiasing = Scene3d.multisampling
                , exposure = Scene3d.exposureValue 6
                , toneMapping = Scene3d.noToneMapping
                , whiteBalance = Light.fluorescent
                , background = Scene3d.backgroundColor Color.black
                , entities = [ basement ]  ++ allStoneEntities model.board
                }
            ]
        ]


decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.orbiting then
        Sub.batch
            [ Browser.Events.onMouseMove decodeMouseMove
            , Browser.Events.onMouseUp (Decode.succeed MouseUp)
            ]
    else
        Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }