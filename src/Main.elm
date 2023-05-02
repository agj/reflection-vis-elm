module Main exposing (main)

import Browser
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes
import Knob exposing (Knob)
import Scene
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttr


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
                (Knob.intConstrained
                    { range = ( 0, 10 )
                    , step = 1
                    , initial = 5
                    }
                )
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
        , Knob.view ControlsKnobUpdated model.controls
        , Knob.styles
        ]
