module FinishGame exposing (Model, Msg, init, update, view)

import Html exposing (Attribute, Html, button, div, form, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Player exposing (Player, player)
import Utils exposing (discardElem, onEnter)


type alias Model =
    { current : String
    , registered : List Player
    }


type Msg
    = OnUpdateName String
    | OnAddName String
    | OnPopName Int
    | OnFinish


init : List Player -> Model
init ps =
    { current = ""
    , registered = ps
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
                Ok { registered = player x :: model.registered, current = "" }

        OnPopName i ->
            Ok { model | registered = discardElem i model.registered }

        OnFinish ->
            Err model.registered


view : Model -> Html Msg
view model =
    let
        names =
            List.indexedMap viewPlayer model.registered
    in
    div [ class "prepare" ]
        [ div [ class "prepare-players" ] names
        ]


viewPlayer : Int -> Player -> Html Msg
viewPlayer i p =
    div [ class "prepare-player" ]
        [ span []
            [ text (String.fromInt (i + 1))
            , text ". "
            ]
        , span
            []
            [ text p.name ]
        , button [ onClick (OnPopName i) ] [ text "x" ]
        ]
