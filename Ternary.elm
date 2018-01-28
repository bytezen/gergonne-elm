module Ternary exposing (..)

import List.Extra as Extra 

type Ternary = Ternary String

type Value = Zero | One | Two
type Gergonne = Gergonne Value Value Value
type PlaceValue = Units | Threes | Nines


--toTernary : Int -> Ternary 
toTernary n =
    let
        toTernary_ n acc =
            let
                r = rem n 3 |> toString
                q = n // 3
            in
                case q of
                    0 -> 
                        r ++ acc
                    _ ->
                        toTernary_ q (r ++ acc)
    in
        Ternary <| toTernary_ n "" 


toValue n =
    case n of
        0 -> Ok Zero
        1 -> Ok One
        2 -> Ok Two
        _ -> Err <| "invalid digit " ++ (toString n)

--toGergonne : Int -> Gergonne
toGergonne n =
    let
        r = (rem n 3) --|> Result.withDefault Zero

        q = n // 3

        accum (n,rs) =
            if n <= 0 then
                (n,rs)
            else
                accum (n // 3
                      , [rem n 3] ++ rs
                      )

        remainders (n,rs) = 
            let
                padding = 3 - List.length rs
            in
                (List.repeat padding 0)
                ++ 
                rs --accum >> Tuple.second

        values = List.map toValue 
    in
        --remainders <| accum (n,[])
        case values <| remainders <| accum (n,[]) of
            Ok n :: Ok t :: Ok u :: [] ->
                Gergonne n t u
            _ ->
                Gergonne Zero Zero Zero

extractPlace : PlaceValue -> Gergonne -> Value
extractPlace p (Gergonne n t u) =
    case p of 
        Units -> u
        Threes -> t
        Nines -> n

units : Gergonne -> Value
units  n = extractPlace Units n 


threes : Gergonne -> Value
threes n = extractPlace Threes n
            

nines : Gergonne -> Value
nines n = extractPlace Nines n


--accum : Int -> Int -> Result a Int
--accum total n =
--    Result.Ok (3*total + n)


--uncons : Ternary -> Maybe (Int, Ternary)
--uncons (Ternary x) = 
--             case String.uncons x of
--                Just (d,ds) -> 
--                    Just ( Result.withDefault 0 
--                            <| (String.fromChar >> String.toInt) d
--                        , Ternary ds)
--                Nothing ->
--                        Just (0,Ternary "0")



--foo : Ternary -> (Ternary -> (Int, Ternary))
--foo num =
--    let
--        fn accum aTernary =
--            let
--                (d,_) = Maybe.withDefault (0,Ternary "0")
--                            <| uncons aTernary
--            in
--                3 * accum + d 
--    in
            
--    case uncons num of
--        Just (d,(Ternary tNum)) ->
--            fn d


fromTernary : Ternary -> Result.Result String Int
fromTernary (Ternary n) =
    let
        accum : Int -> Int -> Result a Int
        accum total n =
            Result.Ok (3*total + n)

        step tdigit res =
            case String.toInt tdigit of
                Ok n ->
                    Ok (3 * res + n) 
                _ ->
                    Err <| "bad ternary digit: " ++ tdigit
    in
        Ok 10            

