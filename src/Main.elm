module Main exposing (..)


import Angle exposing (Angle)
import Browser
import Browser.Events
import Element as E
import Element.Background as Background
import Element.Font as Font
import Color
import Html exposing (Html)
import Html.Events
import Html exposing (div, text)
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import Scene3d
import Scene3d.Light as Light exposing (Light)
import Svg exposing (Svg)
import Svg.Attributes as SvgAtr
import Task
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)

import Models exposing (Model)
import Objects exposing (..)
import Board exposing (Board)
import Scene3d exposing (Background)
import Browser.Dom exposing (Viewport)
import Models exposing (ViewportSize)


type Msg
    = MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | PutStone
    | ViewportResize ViewportSize


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
        PutStone ->
            ( { model | board = Board.initBoard }, Cmd.none )
        ViewportResize size ->
            ( {model | viewportSize = size }, Cmd.none )

type alias ViewportSize =
    { width : Int, height : Int }

toViewportSize : Browser.Dom.Viewport -> ViewportSize
toViewportSize { viewport } =
    { width = floor viewport.width
    , height = floor viewport.height
    }

init : () -> ( Model, Cmd Msg )
init () =
    ( { azimuth = Angle.degrees 45
      , elevation = Angle.degrees 40
      , orbiting = False
      , board = Board.initBoard
      , selected = -1
      , viewportSize = { width = 600, height = 400 }
      }
    , Task.perform (ViewportResize << toViewportSize) Browser.Dom.getViewport
    )


-- View

view : Model -> Html Msg
view model =
    let
        w = model.viewportSize.width
        h = model.viewportSize.height
    in
    E.layout []
        (E.column
            [ Background.color (E.rgb255 0 0 0)
            , Font.color (E.rgb255 200 200 200)
            , E.htmlAttribute (Html.Events.onMouseDown MouseDown)
            , E.width (E.px w)
            , E.height (E.px h)
            ]
            [ gameDisplay model w
            , gameController model w (h - (3 * w // 4))
            ]
        )


gameDisplay : Model -> Int -> E.Element Msg
gameDisplay model width =
    E.html (Scene3d.custom
                { lights = Scene3d.twoLights lightBulb overheadLighting
                , camera = camera model
                , clipDepth = Length.meters 0.1
                , dimensions = ( Pixels.int width, Pixels.int (3 * width // 4))
                , antialiasing = Scene3d.multisampling
                , exposure = Scene3d.exposureValue 6
                , toneMapping = Scene3d.noToneMapping
                , whiteBalance = Light.fluorescent
                , background = Scene3d.backgroundColor Color.black
                , entities = 
                    [ basement ]
                    ++ allStoneEntities model.board
                }
           )

str : Int -> String 
str s =
    String.fromInt s


svgCircle : Int -> Int -> Int -> String -> Svg msg
svgCircle cx cy radius color =
    Svg.circle
        [ SvgAtr.cx (str cx)
        , SvgAtr.cy (str cy)
        , SvgAtr.r (str radius)
        , SvgAtr.fill color
        ]
        []


svgSquare : Int -> Int -> Int -> Int -> String -> Float -> Svg msg
svgSquare cx cy size radius color rotate =
    Svg.rect
        [ SvgAtr.x (str (cx - size // 2))
        , SvgAtr.y (str (cy - size // 2))
        , SvgAtr.width (str size)
        , SvgAtr.height (str size)
        , SvgAtr.rx (str radius)
        , SvgAtr.ry (str radius)
        , SvgAtr.transform ("rotate(" ++ (String.fromFloat rotate) ++ ", "
                                      ++ (str cx) ++ ", " 
                                      ++ (str cy) ++ ")")
        , SvgAtr.fill color
        ]
        []


boardController : Model -> Int -> Int -> E.Element msg
boardController model width height =
    let
        boardSize =
            floor ((toFloat height) / (sqrt 2))
        rect = 
            svgSquare
                (width // 2)
                (height // 2)
                boardSize
                (height // 20)
                "rgb(200, 200, 200)"
    in
    E.html (Svg.svg [ SvgAtr.width (str width)
                    , SvgAtr.height (str height)
                    ]
                    [ rect  (Angle.inDegrees model.azimuth) ])


gameController : Model -> Int -> Int -> E.Element Msg
gameController model width height =
    E.row 
        [ E.width (E.px width), E.height (E.px height) ]
        [ boardController model width height]


decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        ([Browser.Events.onResize (\width height -> ViewportResize { width = width, height = height })]
            ++ (if model.orbiting then
                [ Browser.Events.onMouseMove decodeMouseMove
                , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                ]
            else []))


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }