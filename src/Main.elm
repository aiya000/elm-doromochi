module Main exposing (..)

import Html exposing (Html, div, text, img, button)
import Html.Attributes exposing (src, alt, id)
import Html.Events exposing (onClick)
import String exposing (toList, fromList)
import Time exposing (every, second)
import Time.DateTime as DateTime exposing (DateTime, dateTime, addSeconds)


{-| Format a `DateTime` to like "02:33:06"
("hours:minutes:second", two digits, zero padded)
-}
timeFormat : DateTime -> String
timeFormat x =
    let
        hour =
            DateTime.hour x |> toString

        minute =
            DateTime.minute x |> toString

        second =
            DateTime.second x |> toString

        twoDigits x =
            case toList x of
                z :: [] ->
                    fromList [ '0', z ]

                _ ->
                    x
    in
        twoDigits hour ++ ":" ++ twoDigits minute ++ ":" ++ twoDigits second


{-| Mean both a Model's state and a Msg.

  - TimerStart: the timer has started | do start the timer
  - TimerStop: the timer has stopped | do stop the timer
  - TimerReset: the timer has been required to reset | do reset the timer to 00:00:00 (hour:min:sec)

-}
type TimerStep
    = TimerStart
    | TimerStop
    | TimerReset


{-| A state.

  - `clock` is a unique timer in this app,
    means what seconds has elapsed since it did `TimerStart`,
    counted up a second by every second
  - `timerState` means what `clock` should do

-}
type alias Model =
    { clock : DateTime
    , timerState : TimerStep
    }


init : ( Model, Cmd Msg )
init =
    { clock = dateTime DateTime.zero
    , timerState = TimerStop
    }
        ! [ Cmd.none ]


{-| TimerCountUp increments `Model.clock` a second
-}
type Msg
    = TimerAction TimerStep
    | TimerCountUp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimerAction TimerStart ->
            { model | timerState = TimerStart } ! [ Cmd.none ]

        TimerAction TimerReset ->
            { model
                | clock = dateTime DateTime.zero
                , timerState = TimerStop
            }
                ! [ Cmd.none ]

        TimerAction TimerStop ->
            { model | timerState = TimerStop } ! [ Cmd.none ]

        TimerCountUp ->
            { model | clock = addSeconds 1 model.clock } ! [ Cmd.none ]


view : Model -> Html Msg
view model =
    div []
        [ img [ src "./src/images/rest.png", alt "zunko", id "zunko" ] []
        , div [ id "clock" ] [ text <| timeFormat model.clock ]
        , div []
            [ button [ onClick <| TimerAction TimerStart, id "start_button" ] [ text "Start" ]
            , button [ onClick <| TimerAction TimerStop, id "stop_button" ] [ text "Stop" ]
            , button [ onClick <| TimerAction TimerReset, id "reset_button" ] [ text "Reset" ]
            ]
        ]


{-| Increment a clock of `Model` at every seconds,
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model.timerState of
        TimerStop ->
            Sub.none

        TimerReset ->
            Sub.none

        TimerStart ->
            every second <| always TimerCountUp


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
