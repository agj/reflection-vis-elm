module Main exposing (main)

import Angle
import Axis2d exposing (Axis2d)
import Browser
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


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


init : Model
init =
    { controls =
        Knob.compose Controls
            |> Knob.stackLabel "Mirror angle"
                (Knob.floatSlider
                    { range = ( -180, 180 )
                    , step = 0.1
                    , initial = 0
                    }
                )
            |> Knob.stackLabel "Sight angle"
                (Knob.floatSlider
                    { range = ( -180, 180 )
                    , step = 0.1
                    , initial = 0
                    }
                )
    }



-- MODEL


type alias Model =
    { controls : Knob Controls }


type alias Controls =
    { mirrorAngle : Float
    , sightAngle : Float
    }



-- UPDATE


type Msg
    = ControlsKnobUpdated (Knob Controls)


update : Msg -> Model -> Model
update msg model =
    case msg of
        ControlsKnobUpdated knobState ->
            { model | controls = knobState }



-- VIEW


view : Model -> Html Msg
view model =
    Html.div [ Html.Attributes.class "container" ]
        [ Svg.svg [ SvgAttr.viewBox -500 -500 1000 1000 ]
            (viewScene (Knob.value model.controls))
        , Html.node "style"
            []
            [ Html.text
                """
                html,
                body,
                .container,
                svg {
                    height: 100%;
                    width: 100%;
                }  
                """
            ]
        , Knob.view ControlsKnobUpdated model.controls
        , Knob.styles
        ]


viewScene : Controls -> List (Html Msg)
viewScene controls =
    let
        eye : Point2d Pixels c
        eye =
            Point2d.pixels -100 -100

        sightDirection : Direction2d c
        sightDirection =
            Direction2d.degrees controls.sightAngle

        sightLine : List (LineSegment2d Pixels c)
        sightLine =
            actualSightLine eye sightDirection mirrorAxis

        mirrorAxis : Axis2d Pixels c
        mirrorAxis =
            Axis2d.through
                Point2d.origin
                Direction2d.positiveY
                |> Axis2d.rotateBy (Angle.degrees controls.mirrorAngle)

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
    [ mirror |> viewMirror
    , box |> viewBox
    , reflectedBox |> viewBox
    ]
        ++ viewSight sightLine
        ++ [ viewEye eye ]


actualSightLine : Point2d Pixels c -> Direction2d c -> Axis2d Pixels c -> List (LineSegment2d Pixels c)
actualSightLine eye direction mirrorAxis =
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
                    Angle.degrees 270
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


viewMirror : LineSegment2d u c -> Html Msg
viewMirror segment =
    segment
        |> Svg.lineSegment2d
            [ SvgAttr.stroke (Paint Color.lightPurple)
            , SvgAttr.strokeWidth (px 3)
            , SvgAttr.strokeLinecap StrokeLinecapRound
            ]


viewBox : Rectangle2d u c -> Html Msg
viewBox rectangle =
    rectangle
        |> Svg.rectangle2d
            [ SvgAttr.fill (Paint Color.lightPurple)
            ]


viewEye : Point2d Pixels c -> Html Msg
viewEye point =
    Circle2d.atPoint point (Pixels.float 10)
        |> Svg.circle2d
            [ SvgAttr.fill (Paint Color.lightPurple)
            ]


viewSight : List (LineSegment2d Pixels c) -> List (Html Msg)
viewSight lines =
    lines
        |> List.map
            (Svg.lineSegment2d
                [ SvgAttr.strokeWidth (px 3)
                , SvgAttr.strokeLinecap StrokeLinecapRound
                , SvgAttr.stroke (Paint Color.lightBrown)
                ]
            )
