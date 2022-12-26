module Main exposing (main)

import Bet
import Browser
import FinishGame
import FinishRound
import Html exposing (Html, a, div, h2, i, text)
import Html.Attributes exposing (class, href, style)
import Player
import Prepare
import Utils exposing (rotateList)


type Model
    = Prepare Prepare.Model
    | Bet Bet.Model
    | FinishRound FinishRound.Model
    | FinishGame FinishGame.Model
    | Invalid


type Msg
    = OnPrepare Prepare.Msg
    | OnBet Bet.Msg
    | OnFinishRound FinishRound.Msg
    | OnFinishGame FinishGame.Msg


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


init : Model
init =
    Prepare (Prepare.init [])


update : Msg -> Model -> Model
update msg model =
    case ( msg, model ) of
        ( OnPrepare msg_, Prepare model_ ) ->
            case Prepare.update msg_ model_ of
                Ok m ->
                    Prepare m

                Err lst ->
                    Bet (Bet.init lst)

        ( OnBet msg_, Bet model_ ) ->
            case Bet.update msg_ model_ of
                Ok m ->
                    Bet m

                Err lst ->
                    FinishRound
                        (lst
                            |> List.map (Player.withBet (\( i, _ ) -> ( i, i )))
                            |> List.reverse
                            |> FinishRound.init
                        )

        ( OnFinishRound msg_, FinishRound model_ ) ->
            case FinishRound.update msg_ model_ of
                Ok m ->
                    FinishRound m

                Err lst ->
                    if Player.isFinishedGame lst then
                        FinishGame (FinishGame.init lst)

                    else
                        Bet
                            (lst
                                |> rotateList
                                |> Bet.init
                            )

        ( OnFinishGame msg_, FinishGame model_ ) ->
            case FinishGame.update msg_ model_ of
                Ok m ->
                    FinishGame m

                Err m ->
                    Prepare (Prepare.init (List.map .name m))

        _ ->
            Invalid


view : Model -> Html Msg
view model =
    let
        bimap f ( a, b ) =
            ( f a, f b )

        ( body, action ) =
            case model of
                Prepare m ->
                    Prepare.view m |> bimap (Html.map OnPrepare)

                Bet m ->
                    Bet.view m |> bimap (Html.map OnBet)

                FinishRound m ->
                    FinishRound.view m |> bimap (Html.map OnFinishRound)

                FinishGame m ->
                    FinishGame.view m |> bimap (Html.map OnFinishGame)

                Invalid ->
                    ( div [] [ text "Estado inválido!" ]
                    , div [] []
                    )
    in
    div [ class "app" ]
        [ header
        , div [ style "overflow-y" "auto" ] [ body ]
        , div [] [ action ]
        ]


header : Html a
header =
    div [ class "header" ]
        [ h2 [ class "topic" ] [ a [ href "#Der Trumpften" ] [ text "# " ], text "der trumpften" ]

        -- , div [ class "logo" ] [ i [ class "nes-icon trophy" ] [] ]
        ]
