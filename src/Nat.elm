module Nat exposing (..)

{-| Natural numbers
-}


type Nat
    = Z
    | S Nat


{-| under -1 and 0 are mapped to `Z`
-}
fromInt : Int -> Nat
fromInt x =
    if x <= 0 then
        Z
    else
        S <| fromInt <| x - 1
