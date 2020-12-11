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
import Svg.Attributes as SA
import Svg.Events as SE
import Task
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)

import Models exposing (Model)
import Objects exposing (..)
import Board as B exposing (Board)
import Scene3d exposing (Background)
import Browser.Dom exposing (Viewport)
import Models exposing (ViewportSize)


type Msg
    = MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseOver (Int, Int)
    | ViewportResize ViewportSize


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        MouseDown ->
            if model.selected == (-1, -1)then
                ( { model | orbiting = True }, Cmd.none )
            else
                ( { model | board = B.setStone model.selected B.Black model.board}
                , Cmd.none)
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
        MouseOver id ->
            ( { model | selected = id }, Cmd.none )
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
      , board = B.initBoard
      , selected = (-1, -1)
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
            [ Background.color (E.rgb255 255 255 255)
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
                , background = Scene3d.backgroundColor Color.white
                , entities =
                    [ basement ]
                    ++ allStoneEntities model.board
                }
           )

gameController : Model -> Int -> Int -> E.Element Msg
gameController model width height =
    E.row
        [ E.width (E.px width), E.height (E.px height) ]
        [ boardController model width height]


boardController : Model -> Int -> Int -> E.Element Msg
boardController model width height =
    let
        boardSize =
            floor ((toFloat (min width height)) / (sqrt 2))
        rect =
            svgSquare
                (width // 2)
                (height // 2)
                boardSize
                (boardSize // 20)
                "rgb(200, 200, 200)"
        buttons =
            svgButtons
                model
                (width // 2)
                (height // 2)
                boardSize
        rotate =
            Angle.inDegrees model.azimuth
    in
    E.html
        <| Svg.svg
            [ SA.width (str width)
            , SA.height (str height)
            ]
            ([ rect rotate ] ++ (buttons rotate))



str : Int -> String
str s =
    String.fromInt s


rotateStr : Int -> Int -> Float -> String
rotateStr cx cy rotate =
    ("rotate("
    ++ (String.fromFloat rotate)
    ++ ", " ++ (str cx) ++ ", "
    ++ (str cy)
    ++ ")")


svgSquare : Int -> Int -> Int -> Int -> String -> Float -> Svg msg
svgSquare cx cy size radius color rotate =
    Svg.rect
        [ SA.x (str (cx - size // 2))
        , SA.y (str (cy - size // 2))
        , SA.width (str size)
        , SA.height (str size)
        , SA.rx (str radius)
        , SA.ry (str radius)
        , SA.transform (rotateStr cx cy rotate)
        , SA.fill color
        ]
        []


svgButtons : Model -> Int -> Int -> Int -> Float -> List (Svg Msg)
svgButtons model cx cy size rotate =
    let
        nx = B.maxX
        ny = B.maxY
        f = toFloat
        initX = cx - round (f size * ((f nx) - 1) / (2 * (f nx + 1)))
        initY = cy - round (f size * ((f ny) - 1) / (2 * (f ny + 1)))
        radius = size // 15
    in
    List.map
        (\n ->
            let
                x = remainderBy nx n
                y = n // nx
            in
            svgCircle
                (x, y)
                (initX + (size // (nx + 1)) * x)
                (initY + (size // (ny + 1)) * y)
                radius
                (if (x, y) ==  model.selected then "rgb(240, 140, 140)" else "rgb(240, 240, 240)")
                (rotateStr cx cy rotate))
        (List.range 0 (nx * ny - 1))


svgCircle : (Int, Int) -> Int -> Int -> Int -> String -> String -> Svg Msg
svgCircle id cx cy radius color rotate =
    Svg.circle
        [ SA.cx (str cx)
        , SA.cy (str cy)
        , SA.r (str radius)
        , SA.fill color
        , SA.transform rotate
        , SE.onMouseOver (MouseOver id)
        , SE.onMouseOut (MouseOver (-1, -1))
        ]
        []


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