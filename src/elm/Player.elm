module Player exposing (Player, addBet, getBets, getWins, mapBet, nBets, nCards, nRound, player, points, popBet, withBet, isFinishedGame)


type alias Player =
    { name : String
    , bets : List ( Int, Int )
    }


player : String -> Player
player name =
    { name = name, bets = [] }


points : Player -> Int
points p =
    let
        do bets pts =
            case bets of
                [] ->
                    pts

                ( x, y ) :: tail ->
                    if x == y then
                        do tail (pts + pointsPerBet x)

                    else if y == -1 then
                        do tail pts

                    else
                        do tail (pts - 2 * abs (x - y))
    in
    do p.bets 0


pointsPerBet : number -> number
pointsPerBet n =
    if n <= 0 then
        5

    else
        2 * n + 7


addBet : Int -> Player -> Player
addBet n p =
    { p | bets = ( n, -1 ) :: p.bets }


popBet : Player -> Player
popBet p =
    { p | bets = List.tail p.bets |> Maybe.withDefault [] }


withBet : (( Int, Int ) -> ( Int, Int )) -> Player -> Player
withBet f p =
    case p.bets of
        ( i, j ) :: rest ->
            { p | bets = f ( i, j ) :: rest }

        _ ->
            p


mapBet : (Int -> Int -> a) -> Player -> a
mapBet f p =
    case p.bets of
        ( i, j ) :: _ ->
            f i j

        _ ->
            f 0 0


nBets : Player -> Int
nBets p =
    p.bets |> List.head |> Maybe.map (\( x, _ ) -> x) |> Maybe.withDefault 0


getWins : List Player -> List Int
getWins ps =
    List.map (mapBet (\_ n -> n)) ps


getBets : List Player -> List Int
getBets ps =
    List.map (mapBet (\n _ -> n)) ps


nRound : Bool -> List Player -> Int
nRound isFinished ps =
    ps
        |> List.map (\p -> List.length p.bets)
        |> List.minimum
        |> Maybe.withDefault 0
        |> (+)
            (if isFinished then
                -1

             else
                0
            )


nCards : Bool -> List Player -> Int
nCards isFinished ps =
    let
        n =
            51 // List.length ps

        total =
            n - nRound isFinished ps
    in
    if total > 0 then
        total

    else
        abs total + 1


isFinishedGame : List Player -> Bool
isFinishedGame ps = 
    nRound True ps + 1>= 2 * (51 // List.length ps)