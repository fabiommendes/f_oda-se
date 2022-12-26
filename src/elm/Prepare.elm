module Prepare exposing (Model, Msg, init, update, view)

import Html exposing (Html, button, div, h2, h3, input, table, tbody, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Player exposing (Player, player)
import Utils exposing (discardElem, iff, onEnter, swapElem)


type alias Model =
    { current : String
    , registered : List Player
    }


type Msg
    = OnUpdateName String
    | OnAddName String
    | OnPopName Int
    | OnUp Int
    | OnDown Int
    | OnFinish


init : List String -> Model
init names =
    { current = ""
    , registered = List.map Player.player names
    }


update : Msg -> Model -> Result (List Player) Model
update msg model =
    case msg of
        OnUpdateName x ->
            Ok { model | current = x }

        OnAddName x ->
            if x == "" then
                Ok { model | current = "" }

            else
                Ok { registered = model.registered ++ [ player x ], current = "" }

        OnPopName i ->
            Ok { model | registered = discardElem i model.registered }

        OnUp 0 ->
            Ok model

        OnUp i ->
            Ok { model | registered = swapElem i model.registered }

        OnDown i ->
            Ok { model | registered = swapElem (i + 1) model.registered }

        OnFinish ->
            Err model.registered


view : Model -> ( Html Msg, Html Msg )
view model =
    let
        name =
            model.current

        names =
            model.registered |> List.indexedMap (viewPlayer size)

        hasRepeatedName =
            List.any (\p -> p.name == model.current) model.registered

        addPlayer =
            div [ class "nes-container wide", style "margin-top" "2em", style "background" "#eee" ]
                [ input [ class "nes-input wide", onInput OnUpdateName, onEnter (OnAddName name), value name, placeholder "Nome" ] []
                , button
                    [ class "nes-btn wide is-primary", onClick (OnAddName name), disabled hasRepeatedName ]
                    [ text "+" ]
                ]

        data =
            if nPlayers > 0 then
                table
                    [ class "nes-table is-centered is-bordered"
                    , style "width" "calc(100% - 0.5rem)"
                    ]
                    [ tr []
                        [ th [ class "nes-text" ] [ text "#" ]
                        , th [] [ text "Nome" ]
                        , th [ class "small" ] [ text "" ]
                        ]
                    , tbody [] names
                    ]

            else
                text " "

        nPlayers =
            List.length model.registered

        enableStart =
            nPlayers >= 2

        startButton =
            button
                [ class "nes-btn wide"
                , class <| iff enableStart "is-primary" "is-disabled"
                , style "margin-top" "2em"
                , onClick OnFinish
                , disabled (not enableStart)
                ]
                [ text "COMEÇAR JOGO!" ]

        size =
            List.length model.registered
    in
    ( div [ class "prepare" ]
        [ h2 [] [ text "Registro de jogadores" ], data ]
    , if size < 10 then
        div [] [ addPlayer, startButton ]

      else
        startButton
    )


viewPlayer : Int -> Int -> Player -> Html Msg
viewPlayer size i p =
    let
        upClass =
            if i == 0 then
                "is-disabled"

            else
                "is-warning"

        downClass =
            if i == size - 1 then
                "is-disabled"

            else
                "is-warning"
    in
    tr []
        [ td [ class "small center" ] [ text (String.fromInt (i + 1)) ]
        , td [ class "wide" ] [ text p.name ]
        , td
            [ class "small"
            , style "white-space" "nowrap"
            ]
            [ button [ onClick (OnDown i), class ("nes-btn " ++ downClass) ] [ text "↓" ]
            , button [ onClick (OnUp i), class ("nes-btn " ++ upClass) ] [ text "↑" ]
            , button [ onClick (OnPopName i), class "nes-btn is-error" ] [ text "x" ]
            ]
        ]
