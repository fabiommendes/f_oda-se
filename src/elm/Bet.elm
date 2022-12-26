module Bet exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Player exposing (Player, player)
import Utils exposing (iff, loader, maybeToList)


type alias Model =
    { done : List Player
    , todo : List Player
    , current : Maybe Player
    , nBets : Int
    , isClosed : Bool
    }


type Msg
    = OnIncrement
    | OnDecrement
    | OnRegister
    | OnFinish
    | OnUndo
    | OnClose


players : Model -> List Player
players model =
    maybeToList model.current ++ model.todo ++ model.done


init : List Player -> Model
init lst =
    { done = []
    , todo = lst |> List.tail |> Maybe.withDefault []
    , current = List.head lst
    , nBets = 0
    , isClosed = False
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

        OnUndo ->
            case ( model.done, model.current ) of
                ( [], _ ) ->
                    Ok model

                ( done :: rest, Nothing ) ->
                    Ok { model | done = rest, current = Just done }

                ( done :: rest, Just current ) ->
                    Ok { model | done = rest, current = Just done, todo = current :: model.todo }

        OnFinish ->
            Err model.done

        OnClose ->
            Ok { model | isClosed = True }


view : Model -> ( Html Msg, Html Msg )
view model =
    let
        action =
            case model.current of
                Just _ ->
                    viewPlaceBetForm model

                Nothing ->
                    viewWaitUntilFinish model
    in
    ( div [ style "overflow-y" "auto" ]
        [ viewDescription model
        , viewPlacedBets model
        ]
    , action
    )


viewWaitUntilFinish : Model -> Html Msg
viewWaitUntilFinish model =
    div []
        [ if model.isClosed then
            div
                [ class "bet-update nes-container wide"
                , style "background" "#eee"
                , style "margin" "2em 0"
                , style "padding" "2em 2em 11em 2em"
                ]
                [ text "Esperando a rodada terminar..."
                , loader
                ]

          else
            button [ class "nes-btn is-error wide", onClick OnUndo ] [ text "oops!" ]
        , button
            [ class "nes-btn is-primary wide"
            , onClick (iff model.isClosed OnFinish OnClose)
            ]
            [ text (iff model.isClosed "terminou..." "começar rodada!") ]
        ]


viewPlaceBetForm : Model -> Html Msg
viewPlaceBetForm model =
    let
        missingBets =
            Player.nCards False (players model) - List.sum (Player.getBets model.done)

        canPlace =
            not (List.isEmpty model.todo) || missingBets /= model.nBets

        canUndo =
            model.done /= []

        placeButton =
            if canPlace then
                button [ class "nes-btn is-primary wide", onClick OnRegister ] [ text "registar aposta" ]

            else
                button [ class "nes-btn is-disabled wide" ] [ text "foda-se!" ]

        undoButton =
            if canUndo then
                button [ class "nes-btn is-error wide", onClick OnUndo ] [ text "oops!" ]

            else
                button [ class "nes-btn is-disabled wide" ] [ text "oops!" ]
    in
    div []
        [ div
            [ class "nes-container wide"
            , style "background" "#eee"
            , style "margin" "0.5em 0"
            , style "display" "flex"
            , style "align-items" "center"
            ]
            [ div [ style "flex" "1" ]
                [ text ((model.current |> Maybe.map .name |> Maybe.withDefault "<...>") ++ ": ") ]
            , div []
                [ button [ class "nes-btn", onClick OnDecrement ] [ text "-" ]
                , text (" " ++ String.fromInt model.nBets ++ " ")
                , button [ class "nes-btn", onClick OnIncrement ] [ text "+" ]
                ]
            ]
        , undoButton
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
            case ( model.current, model.done ) of
                ( Nothing, [] ) ->
                    []

                ( Nothing, x :: xs ) ->
                    Player.popBet x :: xs

                ( Just x, xs ) ->
                    x :: xs ++ model.todo

        ( nRoundDisplay, nCards ) =
            ( Player.nRound False playerList + 1
            , Player.nCards False playerList
            )
    in
    div []
        [ h2 [] [ text "Rodada de apostas" ]
        , h3 [] [ text (String.fromInt nRoundDisplay ++ "a rodada") ]
        , p [] [ text <| String.fromInt (totalBets + model.nBets) ++ " apostas com " ++ String.fromInt nCards ++ " cartas." ]
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
    table
        [ class "nes-table wide is-centered is-bordered"
        , style "width" "calc(100% - 0.5rem)"
        ]
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
        [ td [ class "wide" ] [ text p.name ]
        , td [] [ text (String.fromInt (Player.nBets p)) ]
        , td [] [ text (String.fromInt (Player.points p)) ]
        ]


viewEmptyBet : Int -> Player -> Html Msg
viewEmptyBet _ p =
    tr [ class "bet-done-player nes-text is-disabled" ]
        [ td [ class "wide" ] [ text p.name ]
        , td [] [ text "-" ]
        , td [] [ text (String.fromInt (Player.points p)) ]
        ]
