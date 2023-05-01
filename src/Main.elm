module Main exposing (main)

import Browser
import Html exposing (Html)


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
    Html.text "Nothing yet"
