module Tests exposing (..)

import Html exposing (..)
import Html.Attributes as Html
import Main
import Test exposing (..)
import Test.Html.Query as Q
import Test.Html.Selector as S
import Tuple exposing (first)


view : Test
view =
    describe "view's"
        [ test "clock shows 00:00:00 at first" <|
            \() ->
                Main.view (first Main.init)
                    |> Q.fromHtml
                    |> Q.find [ S.id "clock" ]
                    |> Q.has [ S.text "00:00:00" ]
        , test "image shows zunko that is on rest" <|
            \() ->
                Main.view (first Main.init)
                    |> Q.fromHtml
                    |> Q.find [ S.id "zunko" ]
                    |> Q.has [ S.attribute <| Html.src "./src/images/rest.png" ]
        ]
