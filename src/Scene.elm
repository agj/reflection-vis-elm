module Scene exposing
    ( Config
    , Scene
    , construct
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
import Maybe.Extra as Maybe
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity
import Rectangle2d exposing (Rectangle2d)
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Types exposing (Paint(..), StrokeLinecap(..), Transform(..), pc, px)
import Vector2d


type alias Scene =
    { eye : Point2d Pixels Never
    , actualSightLine : List (LineSegment2d Pixels Never)
    , projectedSightLine : Maybe (LineSegment2d Pixels Never)
    , mirrorRight : LineSegment2d Pixels Never
    , mirrorLeft : LineSegment2d Pixels Never
    , reflectedMirrors : List (LineSegment2d Pixels Never)
    , box : Rectangle2d Pixels Never
    , reflectedBox : Rectangle2d Pixels Never
    }


type alias Config =
    { mirrorAngle : Float
    , sightAngle : Float
    , depth : Int
    }


construct : Config -> Scene
construct { mirrorAngle, sightAngle, depth } =
    let
        eye : Point2d Pixels c
        eye =
            Point2d.pixels -100 -100

        sightDirection : Direction2d c
        sightDirection =
            Direction2d.degrees sightAngle

        sightLine : List (LineSegment2d Pixels c)
        sightLine =
            getActualSightLine eye sightDirection mirrorLeftAxis mirrorRightAxis box

        projectedSightLine : Maybe (LineSegment2d Pixels c)
        projectedSightLine =
            getProjectedSightLine eye sightDirection mirrorLeftAxis mirrorRightAxis reflectedBox

        mirrorRightAxis : Axis2d Pixels c
        mirrorRightAxis =
            Axis2d.through Point2d.origin Direction2d.positiveY
                |> Axis2d.rotateBy (Angle.degrees mirrorAngle)

        mirrorRight : LineSegment2d Pixels c
        mirrorRight =
            axisToMirrorLine mirrorRightAxis

        mirrorLeftAxis : Axis2d Pixels c
        mirrorLeftAxis =
            Axis2d.through
                (Point2d.pixels -200 0)
                Direction2d.positiveY

        mirrorLeft : LineSegment2d Pixels c
        mirrorLeft =
            axisToMirrorLine mirrorLeftAxis

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
                    (Axis2d.throughPoints (LineSegment2d.startPoint mirrorRight) (LineSegment2d.endPoint mirrorRight)
                        |> Maybe.withDefault Axis2d.x
                    )

        reflectedAxes : List (Axis2d Pixels c)
        reflectedAxes =
            reflectAxes depth mirrorRightAxis mirrorLeftAxis

        reflectedMirrors : List (LineSegment2d Pixels c)
        reflectedMirrors =
            reflectedAxes
                |> List.map axisToMirrorLine
    in
    { eye = eye
    , actualSightLine = sightLine
    , projectedSightLine = projectedSightLine
    , mirrorRight = mirrorRight
    , mirrorLeft = mirrorLeft
    , reflectedMirrors = reflectedMirrors
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
    [ viewMirror scene.mirrorRight
    , viewMirror scene.mirrorLeft
    ]
        ++ (scene.reflectedMirrors |> List.map viewMirror)
        ++ [ viewBox scene.box
           , viewBox scene.reflectedBox
           ]
        ++ projectedSightLine
        ++ (scene.actualSightLine |> List.map (viewSightLine True))
        ++ [ viewEye scene.eye ]



-- INTERNAL


axisToMirrorLine : Axis2d Pixels c -> LineSegment2d Pixels c
axisToMirrorLine axis =
    LineSegment2d.along axis
        (Pixels.float -200)
        (Pixels.float 200)


getActualSightLine : Point2d Pixels c -> Direction2d c -> Axis2d Pixels c -> Axis2d Pixels c -> Rectangle2d Pixels c -> List (LineSegment2d Pixels c)
getActualSightLine eye direction mirrorLeftAxis mirrorRightAxis box =
    let
        sightLine : LineSegment2d Pixels c
        sightLine =
            LineSegment2d.fromPointAndVector
                eye
                (Vector2d.withLength (Pixels.float 1000) direction)

        bouncedLine : List (LineSegment2d Pixels c)
        bouncedLine =
            bounceLineBetween 10 sightLine mirrorLeftAxis mirrorRightAxis

        intersectedBouncedLine : List (LineSegment2d Pixels c)
        intersectedBouncedLine =
            bouncedLine
                |> List.foldr
                    (\line accumulated ->
                        case intersectLineWithBox box line of
                            Just intersectedLine ->
                                [ intersectedLine ]

                            Nothing ->
                                line :: accumulated
                    )
                    []
    in
    intersectedBouncedLine


getProjectedSightLine : Point2d Pixels c -> Direction2d c -> Axis2d Pixels c -> Axis2d Pixels c -> Rectangle2d Pixels c -> Maybe (LineSegment2d Pixels c)
getProjectedSightLine eye direction mirrorLeftAxis mirrorRightAxis reflectedBox =
    let
        sightLine : LineSegment2d Pixels c
        sightLine =
            LineSegment2d.fromPointAndVector
                eye
                (Vector2d.withLength (Pixels.float 1000) direction)

        intersection : Maybe (Point2d Pixels c)
        intersection =
            sightLine
                |> LineSegment2d.intersectionWithAxis mirrorRightAxis
    in
    intersection
        |> Maybe.map
            (\point ->
                let
                    line =
                        LineSegment2d.from point (LineSegment2d.endPoint sightLine)
                in
                line
                    |> intersectLineWithBox reflectedBox
                    |> Maybe.withDefault line
            )


bounceLineBetween : Int -> LineSegment2d Pixels c -> Axis2d Pixels c -> Axis2d Pixels c -> List (LineSegment2d Pixels c)
bounceLineBetween maxBounces line bounceAxisA bounceAxisB =
    let
        bounced =
            case bounceLine line bounceAxisA of
                Nothing ->
                    bounceLine line bounceAxisB

                more ->
                    more
    in
    case bounced of
        Just ( lineBeforeBounce, lineAfterBounce ) ->
            let
                restBouncedLines =
                    if maxBounces > 1 then
                        bounceLineBetween (maxBounces - 1) lineAfterBounce bounceAxisA bounceAxisB

                    else
                        []
            in
            lineBeforeBounce :: restBouncedLines

        Nothing ->
            [ line ]


bounceLine : LineSegment2d Pixels c -> Axis2d Pixels c -> Maybe ( LineSegment2d Pixels c, LineSegment2d Pixels c )
bounceLine line bounceAxis =
    line
        |> LineSegment2d.intersectionWithAxis bounceAxis
        |> Maybe.filter ((/=) (LineSegment2d.startPoint line))
        |> Maybe.map
            (\intersectionPoint ->
                let
                    exitAngle =
                        line
                            |> LineSegment2d.mirrorAcross bounceAxis
                            |> LineSegment2d.direction
                            |> Maybe.map Direction2d.toAngle
                            |> Maybe.withDefault (Angle.degrees 0)
                in
                ( LineSegment2d.from
                    (LineSegment2d.startPoint line)
                    intersectionPoint
                , LineSegment2d.fromPointAndVector
                    intersectionPoint
                    (Vector2d.rTheta (Pixels.float 1000) exitAngle)
                )
            )


intersectLineWithBox : Rectangle2d Pixels c -> LineSegment2d Pixels c -> Maybe (LineSegment2d Pixels c)
intersectLineWithBox box line =
    let
        intersectWithSide : LineSegment2d Pixels c -> Maybe (LineSegment2d Pixels c)
        intersectWithSide side =
            LineSegment2d.intersectionPoint line side
                |> Maybe.map
                    (\intersectionPoint ->
                        LineSegment2d.from
                            (LineSegment2d.startPoint line)
                            intersectionPoint
                    )

        intersections : List (LineSegment2d Pixels c)
        intersections =
            Rectangle2d.edges box
                |> List.filterMap intersectWithSide

        getShorter : LineSegment2d Pixels c -> LineSegment2d Pixels c -> LineSegment2d Pixels c
        getShorter lineA lineB =
            if LineSegment2d.length lineA |> Quantity.lessThan (LineSegment2d.length lineB) then
                lineA

            else
                lineB

        result =
            intersections
                |> List.foldl getShorter line
    in
    if result == line then
        Nothing

    else
        Just result


reflectAxes : Int -> Axis2d u c -> Axis2d u c -> List (Axis2d u c)
reflectAxes depth baseAxis axisToReflect =
    if depth == 0 then
        [ baseAxis ]

    else
        let
            reflected =
                axisToReflect
                    |> Axis2d.mirrorAcross baseAxis
        in
        reflected :: reflectAxes (depth - 1) reflected baseAxis


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
    let
        baseStyles =
            [ SvgAttr.strokeWidth (px 3)
            , SvgAttr.strokeLinecap StrokeLinecapRound
            , SvgAttr.stroke (Paint Color.lightBrown)
            ]

        styles =
            if isActual then
                baseStyles

            else
                SvgAttr.strokeDasharray "4 8"
                    :: baseStyles
    in
    Svg.lineSegment2d styles
