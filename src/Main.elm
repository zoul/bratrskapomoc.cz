module Main exposing (..)

import Html exposing (Html)
import Victim exposing (Victim, decodeVictims)
import Http
import Maps
import Maps.Marker as Marker
import Maps.Map as Map
import Maps.Geo


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }



---
--- Model
---


type alias Model =
    { map : Maps.Model Msg
    , victims : Victims
    }


type Victims
    = Loading
    | Loaded (Result String (List Victim))


init : ( Model, Cmd Msg )
init =
    let
        mapServerUrl =
            "https://a.tile.openstreetmap.org/{z}/{x}/{y}.png"

        initialLocation =
            Maps.Geo.latLng 49.5 17

        initialMap =
            Maps.defaultModel
                |> Maps.updateMap (Map.setTileServer mapServerUrl)
                |> Maps.updateMap (Map.setZoom 6 >> Map.moveTo initialLocation)

        requestVictims =
            Http.send ReceiveVictims (Http.getString "data/victims.csv")
    in
        ( { map = initialMap, victims = Loading }, requestVictims )



--
-- Victims
--


receiveVictims : Result Http.Error String -> Result String (List Victim)
receiveVictims =
    Result.mapError toString
        >> Result.andThen decodeVictims


showVictimsOnMap : Maps.Model Msg -> Victims -> Maps.Model Msg
showVictimsOnMap map victims =
    case victims of
        Loaded (Ok victims) ->
            let
                setMarkers x =
                    Maps.updateMarkers (\_ -> x) map
            in
                victims
                    |> List.filterMap .incidentLatLong
                    |> List.map (uncurry Maps.Geo.latLng)
                    |> List.map Marker.create
                    |> setMarkers

        _ ->
            map



--
-- Update
--


type Msg
    = ReceiveVictims (Result Http.Error String)
    | MapsMsg (Maps.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveVictims response ->
            let
                victims =
                    Loaded (receiveVictims response)

                newMap =
                    showVictimsOnMap model.map victims

                newModel =
                    { model | map = newMap, victims = victims }
            in
                ( newModel, Cmd.none )

        MapsMsg msg ->
            model.map
                |> Maps.update msg
                |> Tuple.mapFirst (\map -> { model | map = map })
                |> Tuple.mapSecond (Cmd.map MapsMsg)



---
--- View
---


view : Model -> Html Msg
view model =
    case model.victims of
        Loading ->
            Html.text "Loading…"

        Loaded (Err e) ->
            Html.text ("Error: " ++ e)

        Loaded (Ok _) ->
            Maps.view model.map |> Maps.mapView MapsMsg
