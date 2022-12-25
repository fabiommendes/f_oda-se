module Bet exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Player exposing (Player, player)
import Utils exposing (maybeToList)


type alias Model =
    { done : List Player
    , todo : List Player
    , current : Maybe Player
    , nBets : Int
    }


type Msg
    = OnIncrement
    | OnDecrement
    | OnRegister
    | OnFinish


players : Model -> List Player
players model =
    maybeToList model.current ++ model.todo ++ model.done


init : List Player -> Model
init lst =
    { done = []
    , todo = lst |> List.tail |> Maybe.withDefault []
    , current = List.head lst
    , nBets = 0
    }


update : Msg -> Model -> Result (List Player) Model
update msg model =
    case msg of
        OnIncrement ->
            Ok { model | nBets = model.nBets + 1 |> Basics.min (players model |> Player.nCards False) }

        OnDecrement ->
            Ok { model | nBets = model.nBets - 1 |> Basics.max 0 }

        OnRegister ->
            case ( model.todo, model.current ) of
                ( [], Just player ) ->
                    Ok
                        { model
                            | done = Player.addBet model.nBets player :: model.done
                            , current = Nothing
                            , nBets = 0
                        }

                ( x :: tail, Just player ) ->
                    Ok
                        { model
                            | done = Player.addBet model.nBets player :: model.done
                            , current = Just x
                            , todo = tail
                            , nBets = 0
                        }

                _ ->
                    Ok model

        OnFinish ->
            Err model.done


view : Model -> Html Msg
view model =
    let
        formData =
            case model.current of
                Just _ ->
                    viewPlaceBetForm model

                Nothing ->
                    viewWaitUntilFinish model
    in
    div [ class "bet" ]
        [ viewDescription model
        , viewPlacedBets model
        , formData
        ]


viewWaitUntilFinish : Model -> Html Msg
viewWaitUntilFinish _ =
    div [ class "bet-update nes-container is-rounded wide", style "background" "#eee", style "margin-top" "2em" ]
        [ text "Esperando a rodada terminar..."
        , button [ class "nes-btn is-warning wide", onClick OnFinish ] [ text "Registar resultados" ]
        ]


viewPlaceBetForm : Model -> Html Msg
viewPlaceBetForm model =
    let
        missingBets =
            Player.nCards False (players model) - List.sum (Player.getBets model.done)

        canPlace =
            not (List.isEmpty model.todo) || missingBets /= model.nBets

        placeButton =
            if canPlace then
                button [ class "nes-btn is-warning", onClick OnRegister ] [ text "Apostar" ]

            else
                button [ class "nes-btn is-disabled" ] [ text "Foda-se!" ]
    in
    div [ class "bet-update nes-container is-rounded wide", style "background" "#eee", style "margin-top" "2em" ]
        [ text ((model.current |> Maybe.map .name |> Maybe.withDefault "<...>") ++ ": ")
        , button [ class "nes-btn", onClick OnDecrement ] [ text "-" ]
        , text (" " ++ String.fromInt model.nBets ++ " ")
        , button [ class "nes-btn", onClick OnIncrement ] [ text "+" ]
        , placeButton
        ]


viewDescription : Model -> Html Msg
viewDescription model =
    let
        totalBets =
            List.sum (Player.getBets model.done)

        -- Quando todos jogadores terminam as apostas, antecipa-se o próximo round
        -- Criamos uma lista sem uma aposta
        playerList =
            case (model.current, model.done) of 
                (Nothing, []) -> []
                (Nothing, x::xs) -> (Player.popBet x) :: xs
                (Just x, xs) -> x :: xs ++ model.todo


        ( nRoundDisplay, nCards ) =
            ( Player.nRound False playerList + 1
            , Player.nCards False playerList
            )
    in
    div []
        [ h2 [] [ text "Rodada de apostas" ]
        , h3 [] [ text (String.fromInt nRoundDisplay ++ "a rodada") ]
        , p [] [ text <| String.fromInt totalBets ++ " apostas com " ++ String.fromInt nCards ++ " cartas." ]
        ]


viewPlacedBets : Model -> Html Msg
viewPlacedBets model =
    let
        missing =
            (model.current |> Maybe.map List.singleton |> Maybe.withDefault []) ++ model.todo

        rows =
            (model.done |> List.reverse |> List.indexedMap viewBet)
                ++ (missing |> List.indexedMap viewEmptyBet)
    in
    table [ class "nes-table wide is-centered is-bordered" ]
        [ tr []
            [ th [] [ text "Nome" ]
            , th [] [ text "Apostas" ]
            , th [] [ text "Pontos" ]
            ]
        , tbody [] rows
        ]


viewBet : Int -> Player -> Html Msg
viewBet _ p =
    tr [ class "bet-done-player" ]
        [ td [] [ text p.name ]
        , td [] [ text (String.fromInt (Player.nBets p)) ]
        , td [] [ text (String.fromInt (Player.points p)) ]
        ]


viewEmptyBet : Int -> Player -> Html Msg
viewEmptyBet _ p =
    tr [ class "bet-done-player nes-text is-disabled" ]
        [ td [] [ text p.name ]
        , td [] [ text "-" ]
        , td [] [ text (String.fromInt (Player.points p)) ]
        ]
