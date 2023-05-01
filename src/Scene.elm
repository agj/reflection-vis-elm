module Scene exposing
    ( construct
    , view
    )

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
    , projectedSightLine : Maybe (LineSegment2d Pixels Never)
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

        projectedSightLine : Maybe (LineSegment2d Pixels c)
        projectedSightLine =
            getProjectedSightLine eye sightDirection mirrorAxis

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
    , projectedSightLine = projectedSightLine
    , mirror = mirror
    , box = box
    , reflectedBox = reflectedBox
    }


view : Scene -> List (Html msg)
view scene =
    let
        projectedSightLine =
            case scene.projectedSightLine of
                Just line ->
                    [ viewSightLine False line ]

                Nothing ->
                    []
    in
    [ viewMirror scene.mirror
    , viewBox scene.box
    , viewBox scene.reflectedBox
    ]
        ++ projectedSightLine
        ++ (scene.actualSightLine |> List.map (viewSightLine True))
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


getProjectedSightLine : Point2d Pixels c -> Direction2d c -> Axis2d Pixels c -> Maybe (LineSegment2d Pixels c)
getProjectedSightLine eye direction mirrorAxis =
    let
        sightLine : LineSegment2d Pixels c
        sightLine =
            LineSegment2d.fromPointAndVector
                eye
                (Vector2d.withLength (Pixels.float 1000) direction)

        intersection : Maybe (Point2d Pixels c)
        intersection =
            sightLine
                |> LineSegment2d.intersectionWithAxis mirrorAxis
    in
    intersection
        |> Maybe.map
            (\line ->
                LineSegment2d.from
                    line
                    (LineSegment2d.endPoint sightLine)
            )


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


viewSightLine : Bool -> LineSegment2d Pixels c -> Html msg
viewSightLine isActual =
    Svg.lineSegment2d
        [ SvgAttr.strokeWidth (px 3)
        , SvgAttr.strokeLinecap StrokeLinecapRound
        , SvgAttr.stroke (Paint Color.lightBrown)
        , SvgAttr.strokeDasharray "4 8"
        ]
