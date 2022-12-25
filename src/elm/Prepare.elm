module Prepare exposing (Model, Msg, init, update, view)

import Html exposing (Html, button, div, h1, h2, h3, input, span, table, tbody, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Player exposing (Player, player)
import Utils exposing (discardElem, onEnter, swapElem)


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


view : Model -> Html Msg
view model =
    let
        name =
            model.current

        names =
            model.registered |> List.indexedMap (viewPlayer size)

        hasRepeatedName =
            List.any (\p -> p.name == model.current) model.registered

        spacing =
            div [] []

        addButton =
            div [ class "nes-container is-rounded wide", style "margin-top" "2em", style "background" "#eee" ]
                [ h3 [] [ text "Acrescentar jogadxr" ]
                , input [ class "nes-input wide", onInput OnUpdateName, onEnter (OnAddName name), value name, placeholder "Nome" ] []
                , button
                    [ class "nes-btn wide is-primary", onClick (OnAddName name), disabled hasRepeatedName ]
                    [ text "+" ]
                ]

        data =
            if nPlayers > 0 then
                table [ class "nes-table is-centered is-bordered" ]
                    [ tr []
                        [ th [ class "small" ] [ text "#" ]
                        , th [] [ text "Nome" ]
                        , th [ class "small" ] [ text "" ]
                        , th [ class "small" ] [ text "" ]
                        , th [ class "small" ] [ text "x" ]
                        ]
                    , tbody [] names
                    ]

            else
                text " "

        nPlayers =
            List.length model.registered

        startButton =
            button [ class "nes-btn is-warning wide", style "margin-top" "2em", onClick OnFinish, disabled (nPlayers < 2) ] [ text "COMEÇAR JOGO!" ]

        size =
            List.length model.registered
    in
    div [ class "prepare" ]
        (if size < 10 then
            [ data, spacing, addButton, startButton ]

         else
            [ data, spacing, startButton ]
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
        [ td [ class "small" ] [ text (String.fromInt (i + 1)) ]
        , td [] [ text p.name ]
        , td [ class "small" ] [ button [ onClick (OnDown i), class ("nes-btn " ++ downClass), style "transform" "rotateZ(90deg)" ] [ text ">" ] ]
        , td [ class "small" ] [ button [ onClick (OnUp i), class ("nes-btn " ++ upClass), style "transform" "rotateZ(-90deg)" ] [ text ">" ] ]
        , td [ class "small" ] [ button [ onClick (OnPopName i), class "nes-btn is-error" ] [ text "x" ] ]
        ]
