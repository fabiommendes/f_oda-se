module FinishRound exposing (Model, Msg, init, update, view)

import Debug
import Html exposing (Html, button, div, h2, h3, p, span, strong, table, tbody, td, text, th, tr)
import Html.Attributes exposing (class, disabled, style)
import Html.Events exposing (onClick)
import Player exposing (Player, nBets, withBet)
import Utils exposing (mapAt)


type alias Model =
    List Player


type Msg
    = OnIncrement Int
    | OnDecrement Int
    | OnFinish


init : List Player -> Model
init lst =
    lst


update : Msg -> Model -> Result Model Model
update msg model =
    case msg of
        OnIncrement idx ->
            model
                |> mapAt (withBet (\( i, j ) -> ( i, j + 1 ))) idx
                |> Ok

        OnDecrement idx ->
            model
                |> mapAt (withBet (\( i, j ) -> ( i, j - 1 |> max 0 ))) idx
                |> Ok

        OnFinish ->
            Err model


view : Model -> Html Msg
view model =
    let
        thead =
            tr []
                [ th [] [ text "#" ]
                , th [] [ text "nome" ]
                , th [] [ text "vitórias" ]
                , th [] [ text "pts" ]
                ]

        body =
            List.indexedMap viewPlayer model

        data =
            table [ class "nes-table is-rounded is-bordered" ] [ thead, tbody [] body ]

        nRound =
            Player.nRound True model

        nCards =
            Player.nCards True model

        nWins =
            List.sum (Player.getWins model)

        diff =
            nWins - nCards

        correctMsg =
            p [] [ text "Corrija a lista de vitórias para continuar." ]

        (finishWarning, nextClass, nextLabel) =
            if Player.isFinishedGame model then
                (" (última)", "is-error", "Finalizar jogo!")

            else
                ("", "is-success", "Pŕoxima rodada")

                
    in
    div []
        [ h2 [] [ text "Registrando resultados" ]
        , h3 []
            [ text (String.fromInt (nRound + 1) ++ "a rodada")
            , span [ class "nes-text is-error" ] [ text finishWarning ]
            ]
        -- , text
        --     (Debug.toString
        --         { nWins = nWins
        --         , nCards = nCards
        --         , nRound = nRound
        --         , finish = Player.isFinishedGame model
        --         }
        --     )
        , data
        , div [ class "nes-container is-rounded wide", style "background" "#eee", style "margin-top" "1.5em" ]
            (if diff == 0 then
                [ button [ class <| "nes-btn wide " ++ nextClass , onClick OnFinish, disabled (nCards /= nWins) ] [ text nextLabel ] ]

             else if diff == 1 then
                [ strong [] [ text "1 vitória adicional." ], correctMsg ]

             else if diff > 0 then
                [ strong [] [ text (String.fromInt diff ++ " vitórias adicionais.") ], correctMsg ]

             else if diff == -1 then
                [ strong [] [ text "1 vitória a menos." ], correctMsg ]

             else
                [ strong [] [ text (String.fromInt -diff ++ " vitórias a menos.") ], correctMsg ]
            )
        ]


viewPlayer : Int -> Player -> Html Msg
viewPlayer i p =
    let
        ( m, n ) =
            List.head p.bets |> Maybe.withDefault ( 0, 0 )

        cls =
            if Player.mapBet (\a b -> a /= b) p then
                "bad-bet"

            else
                "good-bet"
    in
    tr [ class cls ]
        [ td [] [ text (String.fromInt (i + 1)) ]
        , td [] [ text p.name ]
        , td []
            [ button [ class "nes-btn is-primary", onClick (OnDecrement i) ] [ text "-" ]
            , text (" " ++ String.fromInt n ++ "/" ++ String.fromInt m ++ " ")
            , button [ class "nes-btn is-primary", onClick (OnIncrement i) ] [ text "+" ]
            ]
        , td [] [ text (String.fromInt (Player.points p)) ]
        ]
