module Objects exposing (..)

import Angle exposing (Angle)
import Block3d
import Camera3d exposing (Camera3d)
import Color
import Cylinder3d exposing (Cylinder3d)
import Direction3d exposing (Direction3d)
import LuminousFlux
import Illuminance
import Length exposing (Meters)
import Scene3d
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material exposing (Material)
import Sphere3d exposing (Sphere3d)
import Point3d
import Viewpoint3d exposing (Viewpoint3d)

import Models exposing (Model)
import Board exposing (Board)
import Array exposing (isEmpty)

type WorldCoordinates
    = WorldCoordinates


-- Materials --


white : Material.Uniform WorldCoordinates
white =
    Material.nonmetal
        { baseColor = Color.white
        , roughness = 0.5
        }

black : Material.Uniform WorldCoordinates
black =
    Material.nonmetal
        { baseColor = Color.black
        , roughness = 0.4
        }

alminum : Material.Uniform WorldCoordinates
alminum = 
    Material.nonmetal
        { baseColor = Color.rgb255 230 233 235
        , roughness = 0.1
        }



-- Entities --

stoneRadius : Float
stoneRadius = 0.7

whiteStone : Float -> Float -> Float -> Scene3d.Entity WorldCoordinates
whiteStone x y z =
    Scene3d.sphereWithShadow (Material.uniform white) <|
        Sphere3d.withRadius (Length.centimeters stoneRadius) (Point3d.centimeters x y z)

blackStone : Float -> Float -> Float -> Scene3d.Entity WorldCoordinates
blackStone x y z =
    Scene3d.sphereWithShadow (Material.uniform black) <|
        Sphere3d.withRadius (Length.centimeters stoneRadius) (Point3d.centimeters x y z)

baseWidth : Float
baseWidth = 10

baseHeight : Float
baseHeight = 1

basement : Scene3d.Entity WorldCoordinates
basement =
    let
        halfWidth = baseWidth / 2
    in
    Scene3d.block alminum <|
        Block3d.from
            (Point3d.centimeters halfWidth halfWidth 0)
            (Point3d.centimeters (-halfWidth) (-halfWidth) (-baseHeight))


column : Float -> Float -> Float -> Scene3d.Entity WorldCoordinates
column x y z =
    Scene3d.cylinder (Material.uniform alminum) <|
        Cylinder3d.startingAt 
            (Point3d.centimeters x y z)
            Direction3d.z
            { radius = (Length.centimeters 0.05)  
            , length = (Length.centimeters 6)
            }



stoneCorr : Int -> Int -> Int -> (Float, Float, Float)
stoneCorr x y z =
    let
        f = \n -> (toFloat n)
    in
        ( 0.7 * baseWidth * ((f x) / (f Board.maxX - 1) - 1/2)
        , 0.7 * baseWidth * ((f y) / (f Board.maxY - 1) - 1/2)
        , 0.7 * baseWidth * (f z) / (f Board.maxZ - 1) + stoneRadius
        )


stoneEntity : Int -> Int -> Int -> Board -> List (Scene3d.Entity WorldCoordinates)
stoneEntity x y nCol board =
    let
        (xLength, yLength, zLength) = stoneCorr x y nCol
        entity = case Board.getStoneColor x y nCol board of
            Just Board.White -> [whiteStone xLength yLength zLength]
            Just Board.Black -> [blackStone xLength yLength zLength]
            _ -> []
    in
    if x < Board.maxX && y < Board.maxY && nCol < Board.maxZ then
        entity ++ stoneEntity x y (nCol + 1) board
               ++ (if nCol == 0 then stoneEntity (x + 1) y nCol board else [])
               ++ (if nCol == 0 && x == 0 then stoneEntity x (y + 1) nCol board else [])
    else
        []

allStoneEntities : Board -> List (Scene3d.Entity WorldCoordinates)
allStoneEntities board =
    stoneEntity 0 0 0 board

        

-- Rendering --


camera : Model -> Camera3d Meters WorldCoordinates
camera model =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.orbitZ
            { focalPoint = Point3d.centimeters 0 0 (baseWidth / 3)
            , azimuth = model.azimuth
            , elevation = model.elevation
            , distance = Length.centimeters 35
            }
        , verticalFieldOfView = Angle.degrees 35
        }


lightBulb : Light WorldCoordinates Bool
lightBulb =
    Light.point (Light.castsShadows True)
        { chromaticity = Light.incandescent
        , intensity = LuminousFlux.lumens 300
        , position = Point3d.centimeters 10 10 30
        }

overheadLighting : Light WorldCoordinates Never
overheadLighting =
    Light.overhead
        { upDirection = Direction3d.positiveZ
        , chromaticity = Light.fluorescent
        , intensity = Illuminance.lux 100
        }