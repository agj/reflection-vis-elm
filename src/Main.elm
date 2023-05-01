module Main exposing (main)

import Browser
import Color
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes
import LineSegment2d exposing (LineSegment2d)
import Point2d
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Types exposing (Paint(..), px)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


init : Model
init =
    {}



-- MODEL


type alias Model =
    {}



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    model



-- VIEW


view : Model -> Html Msg
view model =
    Html.div [ Html.Attributes.class "container" ]
        [ Svg.svg
            []
            [ LineSegment2d.from (Point2d.pixels 0 0)
                (Point2d.pixels 100 100)
                |> Svg.lineSegment2d
                    [ SvgAttr.stroke (Paint Color.lightPurple)
                    , SvgAttr.strokeWidth (px 3)
                    ]
            ]
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
        ]
