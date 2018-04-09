module Main exposing (..)
import Html exposing (Html, button, div, text, program, span)
import Html.Events exposing (onClick, on, onWithOptions, onInput)
import Html.Attributes as H exposing (..)
-- import Json.Decode as Decode
-- import Mouse exposing (Position)
import Svg
import Svg.Attributes as S exposing (..)
import Path.LowLevel as LL exposing (Coordinate)
import Time exposing (Time, now)
import Maybe exposing (withDefault)
import Task

import Audio


pomodoroLength : Float
pomodoroLength = 25 * Time.minute

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


-- wave : Coordinate -> Coordinate -> Coordinate -> Coordinate -> Svg.Svg Msg
-- wave endPoint1 controlPoint1 controlPoint2 endPoint2 =
--     let
--         pathSpec = LL.toString [{moveto =  LL.MoveTo LL.Absolute endPoint1,
--                                  drawtos = [LL.CurveTo LL.Absolute [(controlPoint1, controlPoint2, endPoint2)]] } ]
-- in Svg.path [ d pathSpec, stroke "black", fill "none", strokeWidth "3", strokeLinecap "round"] []


-- def f(a,r): return (r*math.cos(a), -r*math.sin(a)+r)

                                    --  arcFlag = LL.SmallestArc,
                                    --  direction = LL.CounterClockwise,

svgArc : Float -> Float -> Html Msg
svgArc clock radius =
    let
        rad = -2.0 * pi * (clock / 12.0 + 0.75)
        x = radius * cos rad
        y = radius * -1.0 * sin rad + radius
        arcFlag = if clock < 6.0 then LL.SmallestArc else LL.LargestArc
        direction = LL.CounterClockwise
        pathSpec = LL.toString [{moveto = LL.MoveTo LL.Absolute (257, 50),
                                 drawtos = [LL.EllipticalArc LL.Relative [{
                                     radii = (200, 200),
                                     xAxisRotate = 0,
                                     arcFlag = arcFlag,
                                     direction = direction,
                                     target = (x, y)
                                 }]]}]
    in Svg.svg
       [S.id "progress-circle", S.width "500", S.height "500", S.viewBox "0 0 500 500"]
       [Svg.path [d pathSpec, stroke "rgba(200,0,0,0.7)", fill "none", strokeWidth "10", strokeLinecap "round"] []]

-- span [ H.class "progress", H.style [("width", (toString progress) ++ "%" )] ] [],

-- VIEW
view : Model -> Html Msg
view model =
    let progress = Maybe.withDefault 0.9999 (Maybe.map (\s -> 1 - s / pomodoroLength) model.secsLeft)
    in
    div [H.class "content"] [
       div [ onClick StartPomodoro, H.class "tomato" ] [
           svgArc (progress * 12) 200,
           div [ H.class "timer-wrapper" ] [
               div [ H.class "timer" ] [(text (formatMillis (withDefault pomodoroLength model.secsLeft)))]
           ]
       ]
    --    div [] [
    --        if isJust model.tPomodoroEnd then
    --        button [onClick ResetPomodoro] [ text "Reset Timer" ]
    --        else span [][]
    --    ]
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
        Just t -> Sub.batch [ Time.every (0.1 * Time.second) Tick ]
        Nothing -> Sub.none

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
