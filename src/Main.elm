module Main exposing (..)

import Html exposing (Html)
import Victim exposing (..)
import Http


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


type Model
    = Loading
    | Loaded (List Victim)
    | Error String


type Msg
    = ReceiveResponse (Result Http.Error String)


loadVictims : Cmd Msg
loadVictims =
    Http.send ReceiveResponse (Http.getString "data/victims.csv")


init : ( Model, Cmd Msg )
init =
    ( Loading, loadVictims )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveResponse result ->
            case result of
                Ok data ->
                    case parseVictims data of
                        Just victims ->
                            ( Loaded victims, Cmd.none )

                        Nothing ->
                            ( Error "CSV parse error", Cmd.none )

                Err e ->
                    ( Error (toString e), Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            Html.text "Loading…"

        Error e ->
            Html.text ("Error: " ++ e)

        Loaded victims ->
            Html.text ("Victims: " ++ toString (List.length victims))
