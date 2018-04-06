module Main exposing (..)
import Html exposing (Html, button, div, text, program, span)
import Html.Events exposing (onClick, on, onWithOptions, onInput)
import Html.Attributes as H exposing (..)
-- import Json.Decode as Decode
-- import Mouse exposing (Position)
-- import Svg
-- import Svg.Attributes as S exposing (..)
-- import Path.LowLevel as LL exposing (Coordinate)
import Time exposing (Time, now)
import Maybe exposing (withDefault)
import Task

import Audio


pomodoroLength : Float
pomodoroLength = 3 * Time.second

-- MODEL

type alias Model = {
    tPomodoroEnd : Maybe Time,
    secsLeft : Maybe Float
}

init : (Model, Cmd Msg)
init =
    let model = 
        {tPomodoroEnd = Nothing,
         secsLeft = Nothing }
    in (model, Cmd.none)

format2digits : Int -> String
format2digits n =
    if n < 10 then "0" ++ (toString n)
    else toString n 

formatMillis : Float -> String
formatMillis dt =
    let n = ceiling (dt / 1000.0)
        minutes = n // 60
        seconds = n % 60
    in (format2digits minutes) ++ ":" ++ (format2digits seconds)

isJust : Maybe a -> Bool
isJust x =
    case x of
    Just _ -> True
    Nothing -> False

-- VIEW
view : Model -> Html Msg
view model =
    let progress = 100 * (Maybe.withDefault 0.3 (Maybe.map (\s -> 1 - s / pomodoroLength) model.secsLeft))
    in
    div [H.class "content"] [
       div [ H.class "progressbar" ] [
           span [ H.class "progress", H.style [("width", (toString progress) ++ "%" )] ] [],
           div [ H.class "timer-wrapper" ] [
               div [ H.class "timer" ] [(text (formatMillis (withDefault 0 model.secsLeft)))]
           ]
       ],
       div [] [
           if isJust model.tPomodoroEnd then
           button [onClick ResetPomodoro] [ text "Reset Timer" ]
           else button [onClick StartPomodoro] [ text "Start Pomodoro" ]
       ]
    ]

type Msg
    = SetTPomodoro Time Float
    | StartPomodoro
    | ResetPomodoro
    | Tick Time

setPomodoro : Time -> Msg
setPomodoro now =
    let secsLeft = pomodoroLength
        t = now + secsLeft
    in SetTPomodoro t secsLeft

updateTick : Time -> Model -> (Model, Cmd Msg)
updateTick now model =
    let secsLeft = Maybe.map (\tEnd -> tEnd - now) model.tPomodoroEnd
    in case secsLeft of
        Just secs ->
            if secs > 0 then ({model | secsLeft=secsLeft}, Cmd.none)
            else ({model | secsLeft=Nothing, tPomodoroEnd=Nothing}, Audio.playSound "ring.ogg")
        Nothing -> (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetTPomodoro t secsLeft -> ({model | tPomodoroEnd = Just t, secsLeft = Just secsLeft}, Cmd.none)
        StartPomodoro -> (model, Task.perform setPomodoro now)
        Tick now -> updateTick now model
        ResetPomodoro -> ({model | tPomodoroEnd = Nothing, secsLeft = Nothing}, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.tPomodoroEnd of
        Just t -> Sub.batch [ Time.every Time.second Tick ]
        Nothing -> Sub.none

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
