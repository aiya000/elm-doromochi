module Main exposing (..)

import Html exposing (Html, div, text, img, button)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Time exposing (every, second)
import Time.DateTime as DateTime exposing (DateTime, dateTime, addSeconds)
import String exposing (toList, fromList)


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
                x :: [] ->
                    fromList [ '0', x ]

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


type Msg
    = TimerAction TimerStep
    | TimerCountUp DateTime


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

        TimerCountUp time ->
            { model | clock = time } ! [ Cmd.none ]


view : Model -> Html Msg
view model =
    div []
        [ img [ src "./src/images/rest.png" ] []
        , div [] [ text <| timeFormat model.clock ]
        , div []
            [ button [ onClick <| TimerAction TimerStart ] [ text "Start" ]
            , button [ onClick <| TimerAction TimerStop ] [ text "Stop" ]
            , button [ onClick <| TimerAction TimerReset ] [ text "Reset" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.timerState of
        TimerStop ->
            Sub.none

        TimerReset ->
            Sub.none

        TimerStart ->
            every second <| always <| TimerCountUp <| addSeconds 1 model.clock


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
