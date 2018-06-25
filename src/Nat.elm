module Nat exposing (..)

import Basics


{-| Natural numbers
-}
type Nat
    = Z
    | S Nat


isZero : Nat -> Bool
isZero n =
    case n of
        Z ->
            True

        S _ ->
            False


{-| under -1 and 0 are mapped to `Z`
-}
fromInt : Int -> Nat
fromInt x =
    if x <= 0 then
        Z
    else
        S <| fromInt <| x - 1


toInt : Nat -> Int
toInt n =
    case n of
        Z ->
            0

        S m ->
            1 + toInt m


{-| `+`
-}
plus : Nat -> Nat -> Nat
plus n m =
    fromInt <| toInt n + toInt m


{-| `*`
-}
times : Nat -> Nat -> Nat
times n m =
    fromInt <| toInt n * toInt m
