module Main exposing (..)

import Html exposing (Html, div, text, img, button)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Time exposing (Time, every, second, inHours, inMinutes, inSeconds)
import Time.Format exposing (format)


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
    { clock : Time
    , timerState : TimerStep
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { clock = 0, timerState = TimerStop }
    in
        ( model, Cmd.none )


type Msg
    = TimerAction TimerStep
    | TimerCountUp Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimerAction TimerStart ->
            ( { model | timerState = TimerStart }, Cmd.none )

        TimerAction TimerReset ->
            let
                model_ =
                    { model | clock = 0 }
            in
                ( { model_ | timerState = TimerStop }, Cmd.none )

        TimerAction TimerStop ->
            ( { model | timerState = TimerStop }, Cmd.none )

        TimerCountUp time ->
            ( { model | clock = time }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ img [ src "./src/images/rest.png" ] []
        , div [] [ text <| format "%H:%M:%S" model.clock ]
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
            every second <| always <| TimerCountUp <| model.clock + second


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
