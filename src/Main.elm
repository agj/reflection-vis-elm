module Main exposing (main)

import Angle
import Browser
import Color
import Direction2d
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes
import LineSegment2d exposing (LineSegment2d)
import Pixels exposing (Pixels)
import Point2d
import Rectangle2d exposing (Rectangle2d)
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Types exposing (Paint(..), Transform(..), pc, px)
import Vector2d


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
    let
        mirror : LineSegment2d Pixels c
        mirror =
            LineSegment2d.from
                (Point2d.pixels 0 -100)
                (Point2d.pixels 0 100)

        box : Rectangle2d Pixels c
        box =
            Rectangle2d.withDimensions
                ( Pixels.float 50, Pixels.float 50 )
                (Angle.degrees 20)
                (Point2d.pixels -100 0)
    in
    Html.div [ Html.Attributes.class "container" ]
        [ Svg.svg [ SvgAttr.viewBox -500 -500 1000 1000 ]
            [ mirror |> viewMirror
            , box |> viewBox
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


viewMirror : LineSegment2d u c -> Html Msg
viewMirror segment =
    segment
        |> Svg.lineSegment2d
            [ SvgAttr.stroke (Paint Color.lightPurple)
            , SvgAttr.strokeWidth (px 3)
            ]


viewBox : Rectangle2d u c -> Html Msg
viewBox rectangle =
    rectangle
        |> Svg.rectangle2d
            [ SvgAttr.fill (Paint Color.lightPurple)
            ]
