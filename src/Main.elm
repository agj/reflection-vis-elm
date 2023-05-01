module Main exposing (main)

import Angle
import Axis2d exposing (Axis2d)
import Browser
import Color
import Direction2d
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes
import Knob exposing (Knob)
import LineSegment2d exposing (LineSegment2d)
import Pixels exposing (Pixels)
import Point2d
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
    }



-- MODEL


type alias Model =
    { controls : Knob Controls }


type alias Controls =
    { mirrorAngle : Float
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
