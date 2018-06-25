module Tests exposing (..)

import Html.Attributes as Html
import Main as M
import Main exposing (Model)
import Nat exposing (..)
import Test exposing (..)
import Test.Html.Query as Q
import Test.Html.Selector as S
import TestExp exposing (..)
import Time.DateTime as DateTime
import Tuple exposing (first)


initialModel : Model
initialModel =
    first M.init


{-| A prescribed value for Model.intervals.workTime
-}
workTime : Nat
workTime =
    initialModel.intervals.workTime


{-| A prescribed value for Model.intervals.restTime
-}
restTime : Nat
restTime =
    initialModel.intervals.restTime


{-| A prescribed value for Model.intervals.cycleLength
-}
cycleLength : Nat
cycleLength =
    initialModel.intervals.cycleLength


{-| A half head time of a working
-}
atWorkHead : DateTime
atWorkHead =
    DateTime.zero


{-| A half tail time of a working
-}
atWorkTail : DateTime
atWorkTail =
    let
        halfTimeOfWork =
            (toInt workTime // 2) + 1
    in
        DateTime.addMinutes halfTimeOfWork atWorkHead


{-| A rest time
-}
atRest : DateTime
atRest =
    DateTime.addMinutes (toInt workTime) DateTime.zero


{-| A long rest time
-}
atLongRest : DateTime
atLongRest =
    let
        oneCycleTime =
            plus workTimel restTime
    in
        DateTime.addMinutes (times oneCycleTime cycleLength) DateTime.zero


view : Test
view =
    let
        -- Zunko has `img` as the src at `time`
        zunkoHas img time =
            \() ->
                M.view { initialModel | clock = time }
                    |> Q.fromHtml
                    |> Q.find [ S.id "zunko" ]
                    |> Q.has [ S.attribute <| Html.src img ]
    in
        describe "view"
            [ test "shows the clock as 00:00:00 at first" <|
                \() ->
                    M.view (first M.init)
                        |> Q.fromHtml
                        |> Q.find [ S.id "clock" ]
                        |> Q.has [ S.text "00:00:00" ]
            , test "shows the image with zunko that takes a rest time, at the rest time" <|
                zunkoHas "./src/images/rest.png" atRest
            , test "shows the image with zunko that works (1), at a head of the working time" <|
                zunkoHas "./src/images/work1.png" atWorkHead
            , test "shows the image with zunko that works (2), at a tail of the working time" <|
                zunkoHas "./src/images/work2.png" atWorkTail
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
