module Main exposing (..)

import Html exposing (Html)
import Victim exposing (..)
import Maps
import Maps.Marker as Marker
import Maps.Map as Map
import Maps.Geo


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { map : Maps.Model Msg
    , victims : Victim.Model
    }


type Msg
    = VictimMsg Victim.Msg
    | MapsMsg (Maps.Msg Msg)


subscriptions : Model -> Sub Msg
subscriptions model =
    Maps.subscriptions model.map
        |> Sub.map MapsMsg


init : ( Model, Cmd Msg )
init =
    ( { map = initialMap, victims = Loading }, Cmd.map VictimMsg loadVictims )


initialMap =
    let
        initialLocation =
            Maps.Geo.latLng 49.5 17
    in
        Maps.defaultModel
            |> Maps.updateMap (Map.setZoom 6 >> Map.moveTo initialLocation)


showVictimsOnMap : Maps.Model Msg -> Victim.Model -> Maps.Model Msg
showVictimsOnMap map victims =
    case victims of
        Loaded victims ->
            let
                pins =
                    victims
                        |> List.filterMap .incidentLatLong
                        |> List.map (uncurry Maps.Geo.latLng)
            in
                Maps.updateMarkers (\_ -> List.map Marker.create pins) map

        _ ->
            map


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VictimMsg msg ->
            let
                showVictims =
                    showVictimsOnMap model.map
            in
                model.victims
                    |> Victim.update msg
                    |> Tuple.mapFirst (\v -> { model | victims = v, map = showVictims v })
                    |> Tuple.mapSecond (Cmd.map VictimMsg)

        MapsMsg msg ->
            model.map
                |> Maps.update msg
                |> Tuple.mapFirst (\map -> { model | map = map })
                |> Tuple.mapSecond (Cmd.map MapsMsg)


view : Model -> Html Msg
view model =
    case model.victims of
        Loading ->
            Html.text "Loading…"

        Error e ->
            Html.text ("Error: " ++ e)

        Loaded _ ->
            Maps.view model.map |> Maps.mapView MapsMsg
