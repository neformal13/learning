module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { counter : Int
    , clicks : Int
    }


init : Model
init =
    { counter = 0, clicks = 0 }



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model
                | counter = model.counter + 1
                , clicks = model.clicks + 1
            }

        Decrement ->
            { model | counter = model.counter - 1, clicks = model.clicks + 1 }

        Reset ->
            { model | counter = 0, clicks = 0 }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button [ onClick Decrement ] [ text "-" ]
            , div [] [ text ("Counter - " ++ String.fromInt model.counter) ]
            , div [] [ text ("Clicks - " ++ String.fromInt model.clicks) ]
            , button [ onClick Increment ] [ text "+" ]
            ]
        , div []
            [ button [ onClick Reset ] [ text "Reset"] ]
        ]
