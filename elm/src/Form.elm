module Form exposing (Model, main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    }


init : Model
init =
    Model "" "" ""



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain passwordAgain ->
            { model | passwordAgain = passwordAgain }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    ul []
        [ if not (String.any Char.isLower model.password) then
            li [ style "color" "red" ] [ text "Password should contain at least one lower case symbol!" ]

          else
            li [ style "color" "green" ] [ text "Password should contain at least one lower case symbol!" ]
        , if not (String.any Char.isUpper model.password) then
            li [ style "color" "red" ] [ text "Password should contain at least one upper case symbol!" ]

          else
            li [ style "color" "green" ] [ text "Password should contain at least one upper case symbol!" ]
        , if String.length model.password < 8 then
            li [ style "color" "red" ] [ text "Password should be longer than 8 symbols!" ]

          else
            li [ style "color" "green" ] [ text "Password contain more than 8 symbols!" ]
        , if model.password /= model.passwordAgain then
            li [ style "color" "red" ] [ text "Password do not match!" ]

          else
            li [ style "color" "green" ] [ text "Password match!" ]
        ]
