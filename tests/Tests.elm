module Tests exposing (..)

import Html exposing (..)
import Html.Attributes as Html
import Main as M
import Test exposing (..)
import Test.Html.Query as Q
import Test.Html.Selector as S
import TestExp exposing (..)
import Time.DateTime as DateTime
import Tuple exposing (first)


view : Test
view =
    describe "view's"
        [ test "clock shows 00:00:00 at first" <|
            \() ->
                M.view (first M.init)
                    |> Q.fromHtml
                    |> Q.find [ S.id "clock" ]
                    |> Q.has [ S.text "00:00:00" ]
        , test "image shows zunko that is on rest" <|
            \() ->
                M.view (first M.init)
                    |> Q.fromHtml
                    |> Q.find [ S.id "zunko" ]
                    |> Q.has [ S.attribute <| Html.src "./src/images/rest.png" ]
        ]


update : Test
update =
    describe "update"
        [ "increments the Model's clock a second with the Msg's timer count up"
            => let
                ( model, _ ) =
                    M.update M.TimerCountUp <| first M.init

                expected =
                    DateTime.addSeconds 1 (first M.init).clock
               in
                model.clock === expected
        , "resets the Model's clock with the Msg's timer reset"
            => let
                ( model, _ ) =
                    M.update (M.TimerAction M.TimerReset) <| first M.init
               in
                model.clock === DateTime.dateTime DateTime.zero
        ]
