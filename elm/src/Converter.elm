module Converter exposing (..)

import Browser
import Html exposing (Attribute, Html, button, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type TemperatureMeasurement
    = Celsius
    | Fahrenheit


type alias Temperature =
    ( TemperatureMeasurement, Float )



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { temperature : Maybe Temperature
    , inputType : TemperatureMeasurement
    , input : String
    }


init : Model
init =
    Model Nothing Celsius ""



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
                , temperature =
                    case String.toFloat newInput of
                        Just temperatureValue ->
                            Just ( model.inputType, temperatureValue )

                        Nothing ->
                            Nothing
            }

        ChangeMeasurement ->
            switchMeasurementType model


switchMeasurementType : Model -> Model
switchMeasurementType model =
    { model
        | inputType =
            case model.inputType of
                Celsius ->
                    Fahrenheit

                Fahrenheit ->
                    Celsius
        , temperature =
            case model.temperature of
                Just ( Celsius, value ) ->
                    Just ( Fahrenheit, value )

                Just ( Fahrenheit, value ) ->
                    Just ( Celsius, value )

                Nothing ->
                    Nothing
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
            viewConverter model.input model.inputType model.temperature "blue" (String.fromFloat (fromCelsiusToFahrenheit celsius))

        Nothing ->
            viewConverter model.input model.inputType model.temperature "red" "???"


viewConverter : String -> TemperatureMeasurement -> Maybe Temperature -> String -> String -> Html Msg
viewConverter userInput inputType temperature color equivalentTemp =
    span []
        [ input [ value userInput, onInput Change, style "width" "40px" ] []
        , text (getTemperatureSign inputType)
        , button [ onClick ChangeMeasurement ] [ text "=" ]
        , span [ style "color" color ]
            [ text
                (case temperature of
                    Just ( Celsius, value ) ->
                        String.fromFloat (fromCelsiusToFahrenheit value)

                    Just ( Fahrenheit, value ) ->
                        String.fromFloat (fromFahrenheitToCelsius value)

                    Nothing ->
                        "???"
                )
            ]
        , text (getTemperatureSign (getOppositeTemperatureMeasurement inputType))
        ]
