module Victim exposing (Victim, decodeVictims)

import Csv exposing (Csv)
import Csv.Decode exposing (..)


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


decodeVictims : String -> Result String (List Victim)
decodeVictims =
    let
        hasName =
            not << String.isEmpty << .name
    in
        Csv.parse
            >> decode decodeVictim
            >> Result.mapError (\_ -> "CSV parse error")
            >> Result.map (List.filter hasName)
