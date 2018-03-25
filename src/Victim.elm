module Victim exposing (Model(..), Msg, Victim, loadVictims, update)

import Csv exposing (Csv)
import Csv.Decode exposing (..)
import Http


type alias LatLong =
    ( Float, Float )


type alias Victim =
    { name : String
    , birthDate : String
    , deathDate : String
    , deathAge : Maybe Int
    , incidentDate : String
    , incidentPlace : String
    , incidentLatLong : Maybe LatLong
    , incidentType : String
    }


type Model
    = Loading
    | Loaded (List Victim)
    | Error String


type Msg
    = ReceiveVictims (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update (ReceiveVictims result) _ =
    case result of
        Ok data ->
            case parseVictims data of
                Just victims ->
                    ( Loaded victims, Cmd.none )

                Nothing ->
                    ( Error "CSV parse error", Cmd.none )

        Err e ->
            ( Error (toString e), Cmd.none )


loadVictims : Cmd Msg
loadVictims =
    Http.send ReceiveVictims (Http.getString "data/victims.csv")


decodeLatLong : String -> Result String LatLong
decodeLatLong str =
    let
        toList =
            identity
                -- "50.0814253N", "14.4876106E"
                >> String.split ", "
                -- "50.0814253", "14.4876106"
                >> List.map (String.dropRight 1)
                -- 50.0814253, 14.4876106
                >> List.filterMap (Result.toMaybe << String.toFloat)
    in
        case toList str of
            [ x, y ] ->
                Ok ( x, y )

            _ ->
                Err "Failed to parse map coordinates"


decodeVictim : Decoder (Victim -> a) a
decodeVictim =
    let
        str =
            (\s -> Ok s)

        int =
            String.toInt
    in
        map Victim
            (next str
                |> andMap (next str)
                |> andMap (next str)
                |> andMap (next (maybe int))
                |> andMap (next str)
                |> andMap (next str)
                |> andMap (next (maybe decodeLatLong))
                |> andMap (next str)
            )


parseVictims : String -> Maybe (List Victim)
parseVictims str =
    let
        hasName =
            not << String.isEmpty << .name

        result =
            Csv.parse str
                |> decode decodeVictim
                |> Result.map (List.filter hasName)
    in
        Result.toMaybe result
