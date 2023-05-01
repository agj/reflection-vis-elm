module Scene exposing (..)

import Angle
import Axis2d exposing (Axis2d)
import Circle2d exposing (Circle2d)
import Color
import Direction2d exposing (Direction2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes
import Knob exposing (Knob)
import LineSegment2d exposing (LineSegment2d)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Rectangle2d exposing (Rectangle2d)
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Types exposing (Paint(..), StrokeLinecap(..), Transform(..), pc, px)
import Vector2d


type alias Scene =
    { eye : Point2d Pixels Never
    , actualSightLine : List (LineSegment2d Pixels Never)
    , projectedSightLine : List (LineSegment2d Pixels Never)
    , mirror : LineSegment2d Pixels Never
    , box : Rectangle2d Pixels Never
    , reflectedBox : Rectangle2d Pixels Never
    }


construct :
    { mirrorAngle : Float
    , sightAngle : Float
    }
    -> Scene
construct { mirrorAngle, sightAngle } =
    let
        eye : Point2d Pixels c
        eye =
            Point2d.pixels -100 -100

        sightDirection : Direction2d c
        sightDirection =
            Direction2d.degrees sightAngle

        sightLine : List (LineSegment2d Pixels c)
        sightLine =
            getActualSightLine eye sightDirection mirrorAxis

        mirrorAxis : Axis2d Pixels c
        mirrorAxis =
            Axis2d.through
                Point2d.origin
                Direction2d.positiveY
                |> Axis2d.rotateBy (Angle.degrees mirrorAngle)

        mirror : LineSegment2d Pixels c
        mirror =
            LineSegment2d.along
                mirrorAxis
                (Pixels.float -200)
                (Pixels.float 200)

        box : Rectangle2d Pixels c
        box =
            Rectangle2d.withDimensions
                ( Pixels.float 50, Pixels.float 50 )
                (Angle.degrees 20)
                (Point2d.pixels -100 0)

        reflectedBox : Rectangle2d Pixels c
        reflectedBox =
            box
                |> Rectangle2d.mirrorAcross
                    (Axis2d.throughPoints (LineSegment2d.startPoint mirror) (LineSegment2d.endPoint mirror)
                        |> Maybe.withDefault Axis2d.x
                    )
    in
    { eye = eye
    , actualSightLine = sightLine
    , projectedSightLine = []
    , mirror = mirror
    , box = box
    , reflectedBox = reflectedBox
    }


view : Scene -> List (Html msg)
view scene =
    [ viewMirror scene.mirror
    , viewBox scene.box
    , viewBox scene.reflectedBox
    ]
        ++ viewSight scene.actualSightLine
        ++ [ viewEye scene.eye ]



-- INTERNAL


getActualSightLine : Point2d Pixels c -> Direction2d c -> Axis2d Pixels c -> List (LineSegment2d Pixels c)
getActualSightLine eye direction mirrorAxis =
    let
        sightLine : LineSegment2d Pixels c
        sightLine =
            LineSegment2d.fromPointAndVector
                eye
                (Vector2d.withLength (Pixels.float 1000) direction)
    in
    bounceLine sightLine mirrorAxis


bounceLine : LineSegment2d Pixels c -> Axis2d Pixels c -> List (LineSegment2d Pixels c)
bounceLine line bounceAxis =
    let
        intersection : Maybe (Point2d Pixels c)
        intersection =
            line
                |> LineSegment2d.intersectionWithAxis bounceAxis
    in
    case intersection of
        Just point ->
            let
                exitAngle =
                    line
                        |> LineSegment2d.mirrorAcross bounceAxis
                        |> LineSegment2d.direction
                        |> Maybe.map Direction2d.toAngle
                        |> Maybe.withDefault (Angle.degrees 0)
            in
            [ LineSegment2d.from
                (LineSegment2d.startPoint line)
                point
            , LineSegment2d.fromPointAndVector
                point
                (Vector2d.rTheta (Pixels.float 1000) exitAngle)
            ]

        Nothing ->
            [ line ]


viewMirror : LineSegment2d u c -> Html msg
viewMirror segment =
    segment
        |> Svg.lineSegment2d
            [ SvgAttr.stroke (Paint Color.lightPurple)
            , SvgAttr.strokeWidth (px 3)
            , SvgAttr.strokeLinecap StrokeLinecapRound
            ]


viewBox : Rectangle2d u c -> Html msg
viewBox rectangle =
    rectangle
        |> Svg.rectangle2d
            [ SvgAttr.fill (Paint Color.lightPurple)
            ]


viewEye : Point2d Pixels c -> Html msg
viewEye point =
    Circle2d.atPoint point (Pixels.float 10)
        |> Svg.circle2d
            [ SvgAttr.fill (Paint Color.lightPurple)
            ]


viewSight : List (LineSegment2d Pixels c) -> List (Html msg)
viewSight lines =
    lines
        |> List.map
            (Svg.lineSegment2d
                [ SvgAttr.strokeWidth (px 3)
                , SvgAttr.strokeLinecap StrokeLinecapRound
                , SvgAttr.stroke (Paint Color.lightBrown)
                ]
            )
