module FinishGame exposing (Model, Msg, init, update, view)

import Html exposing (Html, button, div, h2, h3, i, table, tbody, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Player exposing (Player)


type alias Model =
    List Player


type Msg
    = OnFinish


init : List Player -> Model
init =
    List.sortBy Player.points >> List.reverse


update : Msg -> Model -> Result (List Player) Model
update msg model =
    case msg of
        OnFinish ->
            Err model


view : Model -> Html Msg
view model =
    let
        rows =
            List.indexedMap viewPlayer model
    in
    div []
        [ h2 [] [ text "Resultado" ]
        , h3 [] [ text ("Parabéns " ++ (List.head model |> Maybe.map .name |> Maybe.withDefault "Unknown") ++ "!") ]
        , table
            [ class "nes-table wide is-centered is-bordered" ]
            [ tr []
                [ th [] [ text "#" ]
                , th [] [ text "Nome" ]
                , th [] [ text "Pontos" ]
                ]
            , tbody [] rows
            ]
        , button [ class "nes-btn wide is-primary margin-top", onClick OnFinish ] [ text "Começar outro jogo" ]
        ]


viewPlayer : Int -> Player -> Html Msg
viewPlayer idx p =
    if idx == 0 then
        tr [ class "finish is-success" ]
            [ td [ class "center vpad-lg" ] [ i [ class "nes-icon trophy" ] [] ]
            , td [] [ text p.name ]
            , td [] [ text (String.fromInt (Player.points p)) ]
            ]

    else
        tr [ class "finish" ]
            [ td [ class "center vpad-lg" ] [ text (String.fromInt (idx + 1)) ]
            , td [] [ text p.name ]
            , td [] [ text (String.fromInt (Player.points p)) ]
            ]
