module Main exposing (..)
import Html exposing (Html, button, div, text, program, span)
import Html.Events exposing (onClick, on, onWithOptions, onInput)
import Html.Attributes as H exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Svg
import Svg.Attributes as S exposing (..)
import Path.LowLevel as LL exposing (Coordinate)
import Time exposing (Time, now)
import Maybe exposing (withDefault)
import Task exposing (andThen, Task)
import Http exposing (emptyBody, toTask)
import Erl exposing (Query)
import Erl.Query 

import Audio
import External


pomodoroLength : Float
pomodoroLength = 25 * Time.minute
-- pomodoroLength = 2 * Time.second

-- MODEL

type alias Model = {
    tPomodoroEnd : Maybe Time,
    secsLeft : Maybe Float,
    completedPomodoros : Int,
    showSettings : Bool,
    authToken : Maybe String,
    backupStatus : Maybe Status,
    status : Maybe Status
}

type alias Status = {
    text: String,
    emoji: String,
    expiration: Int
}

pomodoroStatus : Status
pomodoroStatus = {
    text = "Pomdoro!",
    emoji = ":tomato:",
    expiration = 0}

init : (Model, Cmd Msg)
init =
    let model = 
        {tPomodoroEnd = Nothing,
         secsLeft = Nothing,
         showSettings = False,
         authToken = Nothing,
         backupStatus = Nothing,
         status = Nothing,
         completedPomodoros = 0}
    in (model, External.getCurrentUrl ())

decodeStatus : Decode.Decoder Status
decodeStatus =
    Decode.at ["profile"] (
        let text = Decode.at ["status_text"] Decode.string
            emoji = Decode.at ["status_emoji"] Decode.string
            expiration = Decode.at ["status_expiration"] Decode.int
        in
            Decode.map3 Status text emoji expiration
    )

encodeStatus : Status -> String
encodeStatus status =
    Encode.encode 0
        (Encode.object [
            ("status_text", Encode.string status.text),
            ("status_emoji", Encode.string status.emoji )
        ])

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

svgArc : Bool ->  Float -> Html Msg
svgArc running clock =
    let
        radius = 240
        rad = -2.0 * pi * (clock / 12.0 + 0.75)
        x = radius * cos rad
        y = radius * -1.0 * sin rad + radius
        arcFlag = if clock < 6.0 then LL.SmallestArc else LL.LargestArc
        direction = LL.CounterClockwise
        pathSpec = LL.toString [{moveto = LL.MoveTo LL.Absolute (250, 10),
                                 drawtos = [LL.EllipticalArc LL.Relative [{
                                     radii = (radius, radius),
                                     xAxisRotate = 0,
                                     arcFlag = arcFlag,
                                     direction = direction,
                                     target = (x, y)
                                 }]]}]
    in Svg.svg
       [S.id "progress-circle", S.width "500", S.height "500", S.viewBox "0 0 500 500", S.class (if running then "running" else "notrunning") ]
       [Svg.path [d pathSpec, stroke "rgba(200,0,0,0.6)", fill "none", strokeWidth "20", strokeLinecap "round"] []]

isPomodoroRunning : Model -> Bool
isPomodoroRunning model =
    isJust model.tPomodoroEnd

maybe : b -> (a -> b) -> Maybe a -> b
maybe default f m =
    case m of 
        Just v -> f v
        Nothing -> default

-- VIEW
view : Model -> Html Msg
view model =
    let progress = Maybe.withDefault 0.9999 (Maybe.map (\s -> 1 - s / pomodoroLength) model.secsLeft)
    in
    div [H.class "content"] [
        -- button [ H.class "pure-button",  onClick ToggleSettings ] [ text "toggle settings" ],
        -- viewSettings model,
        div [ H.class "debug-window"] [
            div [] [text "Backup Status"],
            div [] [
                text "text: ",
                text (maybe "(Nothing)" (\status -> status.text) model.backupStatus)],
            div [] [
                text "emoji: ",
                text (maybe "(Nothing)" (\status -> status.emoji) model.backupStatus)],
            div [] [text "Slack Status"],
            div [] [
                text "text: ",
                text (maybe "(Nothing)" (\status -> status.text) model.status)],
            div [] [
                text "emoji: ",
                text (maybe "(Nothing)" (\status -> status.emoji) model.status)]
        ],
        div ((if isPomodoroRunning(model) then [] else [onClick StartPomodoro])
             ++ [H.classList [("pomodoro", True), ("notrunning", not (isPomodoroRunning model))]]) [
            svgArc (isPomodoroRunning model) (progress * 12),
            div [H.classList [("timer", True), ("unselectable", True), ("notrunning", not (isPomodoroRunning model))]] [(text (formatMillis (withDefault pomodoroLength model.secsLeft)))]
        ],
        div [H.classList [("buttons-lower", True), ("notrunning", not (isPomodoroRunning model))]] [

           button [H.class "pure-button cancel-button", onClick ResetPomodoro]
                  [ Html.i [H.class "fa fa-cog"] [], text "Cancel Pomodoro" ]
       ],
       div [H.class "completed-pomodoros"] 
           (List.map (\i -> span [H.class "completed-pomodoro"] []) (List.range 1 model.completedPomodoros))
    ]

type Msg
    = SetTPomodoro Time Float
    | StartPomodoro
    | ResetPomodoro
    | Tick Time
    | ToggleSettings
    | UrlUpdate String
    | BackupStatusUpdate Status
    | StatusUpdate Status Status
    | Void     

setPomodoro : Time -> Msg
setPomodoro now =
    let secsLeft = pomodoroLength
        t = now + secsLeft
    in SetTPomodoro t secsLeft

defaultTitle : String
defaultTitle = "Pomodoro"

updateTick : Time -> Model -> (Model, Cmd Msg)
updateTick now model =
    let secsLeft = Maybe.map (\tEnd -> tEnd - now) model.tPomodoroEnd
    in case secsLeft of
        Just secs ->
            if secs > 0 then ({model | secsLeft=secsLeft}, External.title ((formatMillis secs) ++ " - " ++ defaultTitle))
            else ({model | secsLeft=Nothing, tPomodoroEnd=Nothing, completedPomodoros = model.completedPomodoros + 1}, Cmd.batch [External.title defaultTitle, Audio.playSound "ring.ogg"])
        Nothing -> (model, External.title defaultTitle)

authHeader : String -> Http.Header
authHeader authToken = Http.header "Authorization" ("Bearer " ++ authToken)

urlProfileGet : String
urlProfileGet = "https://slack.com/api/users.profile.get"

emptyQuery : Query
emptyQuery = Erl.Query.parse ""

slackUrl : String -> String -> Maybe Query -> String
slackUrl authToken path_ mQuery =
    let url_ = Erl.new
        query = Maybe.withDefault emptyQuery mQuery
        url =
            { url_ |
              protocol = "https"
            , host = ["slack.com"] 
            , query = query
            , path = ["api"] }
                |> Erl.appendPathSegments [path_]
                |> Erl.addQuery "token" authToken
                in Erl.toString url

getStatusUrl : String -> String
getStatusUrl authToken =
    slackUrl authToken "users.profile.get" Nothing

setStatusUrl : String -> Status -> String
setStatusUrl authToken status =
    let profile   = status |> encodeStatus
        query = emptyQuery |> Erl.Query.set "profile" profile
    in slackUrl authToken "users.profile.set" (Just query)

handleGetStatusResponse : Result Http.Error Status -> Msg
handleGetStatusResponse result =
    case result of
        Ok status -> Debug.log (toString status) (BackupStatusUpdate status)
        Err err -> Debug.log ("Errow while getting status: " ++ toString err) Void

getAuthToken : String -> Maybe String
getAuthToken s =
    let l = (Erl.parse s).query |> Erl.Query.getValuesForKey "token"
    in List.head l

setStatusTask : String -> Task Http.Error (Status, Status)
setStatusTask authToken =
    Http.get
      (getStatusUrl authToken)
      decodeStatus
    |> toTask
    |> andThen
    (\backupStatus ->
      Http.post
        (setStatusUrl authToken pomodoroStatus)
        Http.emptyBody
        decodeStatus
      |> Http.toTask
      |> Task.map (\status -> (backupStatus, status)))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetTPomodoro t secsLeft ->
          ({model | tPomodoroEnd = Just t, secsLeft = Just secsLeft},
           model.authToken
           |> maybe
                Cmd.none
                (\authToken ->
                  Task.attempt (\result ->
                      case result of
                        Ok (b, s) -> StatusUpdate b s
                        Err err   -> Void)
                    (setStatusTask authToken)))
        StartPomodoro -> (model, Task.perform setPomodoro now)
        Tick now -> updateTick now model
        ResetPomodoro -> ({model | tPomodoroEnd = Nothing, secsLeft = Nothing}, External.title defaultTitle)
        ToggleSettings -> ({model | showSettings = not model.showSettings}, Cmd.none)
        UrlUpdate url ->  ({model | authToken = getAuthToken url}, Cmd.none)
        BackupStatusUpdate status ->  ({model | backupStatus = Just status}, Cmd.none)
        Void -> (model, Cmd.none)
        StatusUpdate backupStatus status -> ({model | backupStatus = Just backupStatus, status = Just status}, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    let listenUrl = External.currentUrl (\s -> UrlUpdate s)
    in
    case model.tPomodoroEnd of
        Just t -> Sub.batch [ Time.every (0.1 * Time.second) Tick, listenUrl ]
        Nothing -> listenUrl 

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
