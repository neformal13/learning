module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Http



-- API


type Actions
    = GetGist
    | GotGist (Result Http.Error String)


getGist : Cmd Actions
getGist =
    Http.get
        { url = "https://api.github.com/users/mojombo"
        , expect = Http.expectString GotGist
        }



-- todo:


type Model
    = Init
    | Loading
    | Done String
    | Failed


init : () -> ( Model, Cmd Actions )
init _ =
    ( Init, Cmd.none )


view : Model -> Html Actions
view model =
    case model of
        Init ->
            h1 [] [ text "Click the button to download book", button [ onClick GetGist ] [ text "Get book" ] ]

        Loading ->
            h1 [] [ text "Loading" ]

        Done book ->
            article []
                [ h1 [] [ text "Book" ]
                , p [] [ text book ]
                ]

        Failed ->
            h1 [] [ text "Hm... Something went wrong" ]


subscriptions : Model -> Sub Actions
subscriptions _ =
    Sub.none


update : Actions -> Model -> ( Model, Cmd Actions )
update msg model =
    case msg of
        GetGist ->
            ( Loading, getGist )

        GotGist response ->
            case response of
                Ok fullText ->
                    ( Done fullText, Cmd.none )

                Err _ ->
                    ( Failed, Cmd.none )


main =
    Browser.element
        { view = view
        , init = init
        , subscriptions = subscriptions
        , update = update
        }
