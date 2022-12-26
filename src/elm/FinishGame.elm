module FinishGame exposing (Model, Msg, init, update, view)

import Html exposing (Html, button, div, h2, h3, i, span, table, tbody, td, text, th, tr)
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


view : Model -> ( Html Msg, Html Msg )
view model =
    let
        rows =
            List.indexedMap viewPlayer model
    in
    ( div []
        [ h2 [] [ text "Resultado" ]
        , h3 [] [ text ("Parabéns " ++ (List.head model |> Maybe.map .name |> Maybe.withDefault "Unknown") ++ "!") ]
        , table
            [ class "nes-table is-centered is-bordered"
            , style "width" "calc(100% - 0.5rem)"
            ]
            [ tr []
                [ th [] [ text "#" ]
                , th [] [ text "Nome" ]
                , th [] [ text "Pontos" ]
                ]
            , tbody [] rows
            ]
        ]
    , button [ class "nes-btn wide is-primary margin-top", onClick OnFinish ] [ text "Começar outro jogo" ]
    )


viewPlayer : Int -> Player -> Html Msg
viewPlayer idx p =
    if idx == 0 then
        let
            chars =
                String.toList p.name

            n =
                List.length chars

            duration =
                0.25

            step =
                duration / toFloat n

            animateChar i ch =
                span
                    [ style "animation" <| "winner-wave " ++ String.fromFloat duration ++ "s infinite"
                    , style "animation-delay" (String.fromFloat (step * toFloat i) ++ "s")
                    , style "display" "table-cell"
                    , style "font-size" "125%"
                    ]
                    [ text (String.fromChar ch) ]
        in
        tr [ class "finish is-success winner" ]
            [ td [ class "center vpad-lg" ]
                [ div [ class "winner-scale" ] [ i [ class "nes-icon trophy" ] [] ]
                ]
            , td [] <|
                List.indexedMap animateChar chars
            , td [] [ text (String.fromInt (Player.points p)) ]
            ]

    else
        tr [ class "finish" ]
            [ td [ class "center vpad-lg" ] [ text (String.fromInt (idx + 1)) ]
            , td [] [ text p.name ]
            , td [] [ text (String.fromInt (Player.points p)) ]
            ]
