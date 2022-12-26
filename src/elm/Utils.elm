module Utils exposing
    ( discardElem
    , iff
    , mapAt
    , maybeToList
    , onEnter
    , rotateList
    , swapElem, loader
    )

import Html exposing (Attribute, div)
import Html.Attributes exposing (class)
import Html.Events exposing (keyCode, on)
import Json.Decode as Json


{-| Discard i-th element of list, if present
-}
discardElem : Int -> List a -> List a
discardElem idx lst =
    let
        do i head tail =
            case ( i, tail ) of
                ( 0, _ :: rest ) ->
                    List.reverse head ++ rest

                ( _, [] ) ->
                    List.reverse head

                ( _, x :: rest ) ->
                    do (i - 1) (x :: head) rest
    in
    do idx [] lst


{-| Move elem up
-}
swapElem : Int -> List a -> List a
swapElem idx lst =
    let
        pre =
            List.take (idx - 1) lst

        post =
            List.drop (idx - 1) lst
    in
    case post of
        x :: y :: xs ->
            pre ++ (y :: x :: xs)

        _ ->
            lst


mapAt : (a -> a) -> Int -> List a -> List a
mapAt f i lst =
    mapAtAcc f i lst []


mapAtAcc : (a -> a) -> Int -> List a -> List a -> List a
mapAtAcc f i lst acc =
    case ( lst, i ) of
        ( [], _ ) ->
            acc

        ( x :: tail, 0 ) ->
            List.reverse acc ++ (f x :: tail)

        ( x :: tail, _ ) ->
            mapAtAcc f (i - 1) tail (x :: acc)


{-| When the enter key is released, send the `msg`.
Otherwise, do nothing.
-}
onEnter : msg -> Attribute msg
onEnter onEnterAction =
    on "keyup" <|
        Json.andThen
            (\keyCode ->
                if keyCode == 13 then
                    Json.succeed onEnterAction

                else
                    Json.fail (String.fromInt keyCode)
            )
            keyCode


rotateList : List a -> List a
rotateList lst =
    case lst of
        [] ->
            []

        x :: xs ->
            xs ++ [ x ]


maybeToList : Maybe a -> List a
maybeToList mx =
    case mx of
        Just x ->
            [ x ]

        Nothing ->
            []


iff : Bool -> c -> c -> c
iff a b c =
    if a then
        b

    else
        c


loader : Html.Html msg
loader =
    div [ class "loader" ]
        [ div [ class "loader-inner" ]
            [ div [ class "loader-line-wrap" ]
                [ div [ class "loader-line" ]
                    []
                ]
            , div [ class "loader-line-wrap" ]
                [ div [ class "loader-line" ]
                    []
                ]
            , div [ class "loader-line-wrap" ]
                [ div [ class "loader-line" ]
                    []
                ]
            , div [ class "loader-line-wrap" ]
                [ div [ class "loader-line" ]
                    []
                ]
            , div [ class "loader-line-wrap" ]
                [ div [ class "loader-line" ]
                    []
                ]
            ]
        ]
