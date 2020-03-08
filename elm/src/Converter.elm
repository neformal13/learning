module Converter exposing (..)

import Browser
import Html exposing (Attribute, Html, button, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type TemperatureMeasurement
    = Celsius
    | Fahrenheit


type alias Temperature =
    ( TemperatureMeasurement, Maybe Float )



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { temperature : Temperature
    , input : String
    }


init : Model
init =
    Model ( Celsius, Nothing ) ""



-- UPDATE


type Msg
    = Change String
    | ChangeMeasurement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newInput ->
            { model
                | input = newInput
                , temperature = ( Tuple.first model.temperature, String.toFloat newInput )
            }

        ChangeMeasurement ->
            { model
                | temperature =
                    ( case Tuple.first model.temperature of
                        Celsius ->
                            Fahrenheit

                        Fahrenheit ->
                            Celsius
                    , Tuple.second model.temperature
                    )
            }



-- VIEW


fromCelsiusToFahrenheit : Float -> Float
fromCelsiusToFahrenheit celsius =
    celsius * 1.8 + 32


fromFahrenheitToCelsius : Float -> Float
fromFahrenheitToCelsius fahrenheit =
    (fahrenheit - 32) / 1.8


getTemperatureSign : TemperatureMeasurement -> String
getTemperatureSign temperatureType =
    case temperatureType of
        Celsius ->
            "°C"

        Fahrenheit ->
            "°F"


getOppositeTemperatureMeasurement : TemperatureMeasurement -> TemperatureMeasurement
getOppositeTemperatureMeasurement temperatureType =
    case temperatureType of
        Celsius ->
            Fahrenheit

        Fahrenheit ->
            Celsius


view : Model -> Html Msg
view model =
    case String.toFloat model.input of
        Just celsius ->
            viewConverter model.input model.temperature "blue" (String.fromFloat (fromCelsiusToFahrenheit celsius))

        Nothing ->
            viewConverter model.input model.temperature "red" "???"


viewConverter : String -> Temperature -> String -> String -> Html Msg
viewConverter userInput temperature color equivalentTemp =
    span []
        [ input [ value userInput, onInput Change, style "width" "40px" ] []
        , text (getTemperatureSign (Tuple.first temperature))
        , button [ onClick ChangeMeasurement ] [ text "=" ]
        , span [ style "color" color ]
            [ text
                (case temperature of
                    ( _, Nothing ) ->
                        "???"

                    ( Celsius, Just value ) ->
                        String.fromFloat (fromCelsiusToFahrenheit value)

                    ( Fahrenheit, Just value ) ->
                        String.fromFloat (fromFahrenheitToCelsius value)
                )
            ]
        , text (getTemperatureSign (getOppositeTemperatureMeasurement (Tuple.first temperature)))
        ]
