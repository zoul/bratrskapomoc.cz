module Main exposing (..)

import Html exposing (Html)
import Victim exposing (..)
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


type Model
    = Loading
    | Loaded { map : Maps.Model Msg, victims : List Victim }
    | Error String


type Msg
    = ReceiveResponse (Result Http.Error String)
    | MapsMsg (Maps.Msg Msg)


loadVictims : Cmd Msg
loadVictims =
    Http.send ReceiveResponse (Http.getString "data/victims.csv")


init : ( Model, Cmd Msg )
init =
    ( Loading, loadVictims )


buildMap : List Victim -> Maps.Model Msg
buildMap victims =
    let
        initialLocation =
            Maps.Geo.latLng 49.5 17

        pins =
            victims
                |> List.filterMap .incidentLatLong
                |> List.map (uncurry Maps.Geo.latLng)
    in
        Maps.defaultModel
            |> Maps.updateMap (Map.setZoom 6 >> Map.moveTo initialLocation)
            |> Maps.updateMarkers (\_ -> List.map Marker.create pins)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ReceiveResponse result, _ ) ->
            case result of
                Ok data ->
                    case parseVictims data of
                        Just victims ->
                            ( Loaded { map = buildMap victims, victims = victims }, Cmd.none )

                        Nothing ->
                            ( Error "CSV parse error", Cmd.none )

                Err e ->
                    ( Error (toString e), Cmd.none )

        ( MapsMsg msg, Loaded model ) ->
            model.map
                |> Maps.update msg
                |> Tuple.mapFirst (\map -> Loaded { model | map = map })
                |> Tuple.mapSecond (Cmd.map MapsMsg)

        _ ->
            ( Error "Invalid state", Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            Html.text "Loading…"

        Error e ->
            Html.text ("Error: " ++ e)

        Loaded model ->
            Maps.view model.map |> Maps.mapView MapsMsg
