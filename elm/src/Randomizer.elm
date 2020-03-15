{-
   TODO:
    - [x] Instead of showing a number, show the die face as an image.
    - [x] Instead of showing an image of a die face, use elm/svg to draw it yourself.
    - [x] Create a weighted die with Random.weighted.
    - [x] Add a second die and have them both roll at the same time.
    - [ ] Have the dice flip around randomly before they settle on a final value.
-}


module Randomizer exposing (..)

import Browser
import Html exposing (Html, button, div, h1)
import Html.Events exposing (onClick)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Face
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


type alias Model =
    { dieFace : ( Face, Face )
    }


roll : Random.Generator Face
roll =
    Random.weighted
        ( 10, One )
        [ ( 10, Two )
        , ( 10, Three )
        , ( 10, Four )
        , ( 10, Five )
        , ( 10, Six )
        ]


roll2 : Random.Generator ( Face, Face )
roll2 =
    Random.map2 (\first second -> ( first, second )) roll roll


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model ( One, Two )
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewFace ( Face, Face )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewFace roll2
            )

        NewFace newFace ->
            ( Model newFace
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ dieFaceView (Tuple.first model.dieFace)
        , dieFaceView (Tuple.second model.dieFace)
        , button [ onClick Roll ] [ text "Roll" ]
        ]


dieFaceView : Face -> Html msg
dieFaceView num =
    case num of
        One ->
            svg
                [ width "120", height "120", viewBox "0 0 120 120" ]
                [ rect [ x "10", y "10", width "100", height "100", rx "15", ry "15", fill "#ccc" ] []
                , circle [ cx "60", cy "60", r "10", fill "black" ] []
                ]

        Two ->
            svg [ width "120", height "120", viewBox "0 0 120 120" ]
                [ rect [ x "10", y "10", width "100", height "100", rx "15", ry "15", fill "#ccc" ] []
                , circle [ cx "30", cy "30", r "10", fill "black" ] []
                , circle [ cx "90", cy "90", r "10", fill "black" ] []
                ]

        Three ->
            svg [ width "120", height "120", viewBox "0 0 120 120" ]
                [ rect [ x "10", y "10", width "100", height "100", rx "15", ry "15", fill "#ccc" ] []
                , circle [ cx "60", cy "60", r "10", fill "black" ] []
                , circle [ cx "30", cy "30", r "10", fill "black" ] []
                , circle [ cx "90", cy "90", r "10", fill "black" ] []
                ]

        Four ->
            svg [ width "120", height "120", viewBox "0 0 120 120" ]
                [ rect [ x "10", y "10", width "100", height "100", rx "15", ry "15", fill "#ccc" ] []
                , circle [ cx "30", cy "30", r "10", fill "black" ] []
                , circle [ cx "90", cy "30", r "10", fill "black" ] []
                , circle [ cx "30", cy "90", r "10", fill "black" ] []
                , circle [ cx "90", cy "90", r "10", fill "black" ] []
                ]

        Five ->
            svg [ width "120", height "120", viewBox "0 0 120 120" ]
                [ rect [ x "10", y "10", width "100", height "100", rx "15", ry "15", fill "#ccc" ] []
                , circle [ cx "30", cy "30", r "10", fill "black" ] []
                , circle [ cx "90", cy "30", r "10", fill "black" ] []
                , circle [ cx "30", cy "90", r "10", fill "black" ] []
                , circle [ cx "90", cy "90", r "10", fill "black" ] []
                , circle [ cx "60", cy "60", r "10", fill "black" ] []
                ]

        Six ->
            svg [ width "120", height "120", viewBox "0 0 120 120" ]
                [ rect [ x "10", y "10", width "100", height "100", rx "15", ry "15", fill "#ccc" ] []
                , circle [ cx "30", cy "30", r "10", fill "black" ] []
                , circle [ cx "90", cy "30", r "10", fill "black" ] []
                , circle [ cx "30", cy "90", r "10", fill "black" ] []
                , circle [ cx "90", cy "90", r "10", fill "black" ] []
                , circle [ cx "30", cy "60", r "10", fill "black" ] []
                , circle [ cx "90", cy "60", r "10", fill "black" ] []
                ]
