module Victim exposing (Victim, parseVictims)

import Csv exposing (Csv)
import Csv.Decode exposing (..)


type alias Victim =
    { name : String
    , birthDate : String
    , deathDate : String
    , deathAge : Maybe Int
    , incidentDate : String
    , incidentPlace : String
    , incidentGPS : String
    , incidentType : String
    }


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
                |> andMap (next str)
                |> andMap (next str)
            )


parseVictims : String -> Maybe (List Victim)
parseVictims str =
    let
        hasName =
            not << String.isEmpty << .name

        result =
            Csv.parse str |> decode decodeVictim |> Result.map (List.filter hasName)
    in
        Result.toMaybe result
