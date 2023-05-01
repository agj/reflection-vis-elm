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
import Scene
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
        Knob.compose Scene.Config
            |> Knob.stackLabel "Mirror angle"
                (Knob.floatSlider
                    { range = ( -10, 10 )
                    , step = 0.1
                    , initial = 0
                    }
                )
            |> Knob.stackLabel "Sight angle"
                (Knob.floatSlider
                    { range = ( -45, 45 )
                    , step = 0.1
                    , initial = 0
                    }
                )
            |> Knob.stackLabel "Depth"
                (Knob.int { step = 1, initial = 5 })
    }



-- MODEL


type alias Model =
    { controls : Knob Scene.Config }



-- UPDATE


type Msg
    = ControlsKnobUpdated (Knob Scene.Config)


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
            (Scene.construct (Knob.value model.controls)
                |> Scene.view
            )
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
