module Control exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Time exposing (Time, now)
import Maybe exposing (withDefault)
import Task exposing (andThen, Task)
import Http exposing (emptyBody, toTask)
import Erl exposing (Query)
import Erl.Query 
import Audio
import External

import Common exposing (..)


-- MODEL
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
        Ok status -> Debug.log ("hahaha" ++ (toString status)) (BackupStatusUpdate status)
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