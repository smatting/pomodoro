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
import Dict exposing (Dict)
import String
import List exposing (head, tail)
import Maybe exposing (withDefault)

import Common exposing (..)

-- MODEL
pomodoroStatus : Time -> Status
pomodoroStatus tEnd =
  let ft  = formatTime tEnd
      text = "back at " ++ ft
  in {text = text, emoji = ":tomato:", expiration = 0}

init : (Model, Cmd Msg)
init =
  let model = 
    {tPomodoroEnd       = Nothing,
     secsLeft           = Nothing,
     showSettings       = False,
     authToken          = Nothing,
     backupStatus       = Nothing,
     status             = Nothing,
     setPresence        = False,
     presence           = Nothing,
     cookies            = Nothing,
     completedPomodoros = 0}
  in (model, Cmd.batch [External.getCurrentUrl (), External.getCookies ()])

decodeStatus : Decode.Decoder Status
decodeStatus =
  Decode.at ["profile"] <| 
    let text       = Decode.at ["status_text"] Decode.string
        emoji      = Decode.at ["status_emoji"] Decode.string
        expiration = Decode.at ["status_expiration"] Decode.int
    in Decode.map3 Status text emoji expiration
    
encodeStatus : Status -> String
encodeStatus status =
  Encode.encode 0 <|
    Encode.object [
      ("status_text", Encode.string status.text),
      ("status_emoji", Encode.string status.emoji)]

setPomodoro : Time -> Msg
setPomodoro now =
  let secsLeft = pomodoroLength
      t        = now + secsLeft
  in SetTPomodoro t secsLeft

defaultTitle : String
defaultTitle = "Pomodoro"

emptyQuery : Query
emptyQuery = Erl.Query.parse ""

slackUrl : String -> String -> Maybe Query -> String
slackUrl authToken path_ mQuery =
    let url_ = Erl.new
        query = Maybe.withDefault emptyQuery mQuery
        url =
            { url_ |
              protocol = "https"
            , host     = ["slack.com"] 
            , query    = query
            , path     = ["api"] }
                |> Erl.appendPathSegments [path_]
                |> Erl.addQuery "token" authToken
                in Erl.toString url

getStatusUrl : String -> String
getStatusUrl authToken =
    slackUrl authToken "users.profile.get" Nothing

setStatusUrl : String -> Status -> String
setStatusUrl authToken status =
    let profile = status |> encodeStatus
        query   = emptyQuery |> Erl.Query.set "profile" profile
    in slackUrl authToken "users.profile.set" (Just query)

setPresenceUrl : String -> Presence -> String
setPresenceUrl authToken presence =
    let p     = case presence of
                  Auto -> "auto"
                  Away -> "away"
        query = emptyQuery |> Erl.Query.set "presence" p
    in slackUrl authToken "users.setPresence" (Just query)

getAuthTokenFromUrl : String -> Maybe String
getAuthTokenFromUrl s =
    let l = (Erl.parse s).query |> Erl.Query.getValuesForKey "token"
    in List.head l

getAwaySettingFromUrl : String -> Bool
getAwaySettingFromUrl s =
    let l = (Erl.parse s).query |> Erl.Query.getValuesForKey "setPresence"
    in Maybe.withDefault False <| Maybe.map (\s -> s == "true") (List.head l)

getSetStatusTask : String -> Status -> Task Http.Error (Status, Status)
getSetStatusTask authToken targetStatus =
    Http.get
      (getStatusUrl authToken)
      decodeStatus
    |> toTask
    |> andThen
    (\backupStatus ->
      Http.post
        (setStatusUrl authToken targetStatus)
        Http.emptyBody
        decodeStatus
      |> Http.toTask
    |> Task.map (\status -> (backupStatus, status)))

setStatusTask : String -> Status -> Task Http.Error Status
setStatusTask authToken targetStatus =
    Http.post
      (setStatusUrl authToken targetStatus)
      Http.emptyBody
      decodeStatus
    |> Http.toTask

setPresence : String -> Presence -> Cmd Msg
setPresence authToken presence =
  Task.attempt
      (\result ->
        case result of
          Ok presence -> PresenceUpdate presence
          Err err     -> Void)
      (Http.post
        (setPresenceUrl authToken presence)
        Http.emptyBody
        (Decode.succeed presence)
      |> Http.toTask)

getSetStatus : String -> Status -> Cmd Msg
getSetStatus authToken status =
  Task.attempt
      (\result ->
        case result of
          Ok (b, s) -> StatusUpdate (Just b) (Just s)
          Err err   -> Void)
      (getSetStatusTask authToken status)

setStatus : String -> Status -> Cmd Msg
setStatus authToken status =
  Task.attempt
      (\result ->
        case result of
          Ok status -> StatusUpdate Nothing (Just status)
          Err err   -> Void)
      (setStatusTask authToken status)

maybeOverride : Maybe a -> Maybe a -> Maybe a
maybeOverride default maybeValue =
  case maybeValue of
    Nothing -> default
    Just x  -> Just x
  
maybeCmd : (a -> Cmd msg) -> Maybe a -> Cmd msg
maybeCmd f m =
  Maybe.withDefault
    Cmd.none
    (Maybe.map f m)

maybePair : Maybe a -> Maybe b -> Maybe (a, b)
maybePair ma mb =
  Maybe.map2 (\a b -> (a, b)) ma mb

maybeRestoreStatus : Model -> Cmd Msg
maybeRestoreStatus model =
  maybeCmd
    (\(authToken, status) -> setStatus authToken status)
    (maybePair model.authToken model.backupStatus)

maybeSetPresence : Model -> Presence -> Cmd Msg
maybeSetPresence model presence =
  maybeCmd
    (\authToken ->
     if model.setPresence
       then setPresence authToken presence
       else Cmd.none)
    model.authToken

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetTPomodoro t secsLeft          -> ({model | tPomodoroEnd = Just t, secsLeft = Just secsLeft},
                                         Cmd.batch [model.authToken |> maybeCmd (\authToken ->
                                                      getSetStatus authToken (pomodoroStatus t)),
                                                    maybeSetPresence model Away])
    StartPomodoro                    -> (model, Task.perform setPomodoro now)
    Tick now                         -> updateTick now model
    ResetPomodoro                    -> ({model | tPomodoroEnd = Nothing, secsLeft = Nothing},
                                         Cmd.batch [External.title defaultTitle,
                                                    maybeRestoreStatus model,
                                                    maybeSetPresence model Auto])
    ToggleSettings                   -> ({model | showSettings = not model.showSettings}, Cmd.none)
    UrlUpdate url                    -> ({model | authToken = getAuthTokenFromUrl url,
                                                  setPresence = getAwaySettingFromUrl url}, Cmd.none)
    Void                             -> (model, Cmd.none)
    StatusUpdate backupStatus status -> ({model | backupStatus = maybeOverride model.backupStatus backupStatus,
                                                  status = maybeOverride model.status status},
                                         Cmd.none)
    PresenceUpdate presence          -> ({model | presence = Just presence}, Cmd.none)
    CookieUpdate cookies             -> (handleCookies cookies model, Cmd.none)

updateTick : Time -> Model -> (Model, Cmd Msg)
updateTick now model =
  let secsLeft = Maybe.map (\tEnd -> tEnd - now) model.tPomodoroEnd
  in case secsLeft of
    Just secs ->
      if secs > 0
      then ({model | secsLeft = secsLeft},
            External.title ((formatDuration secs) ++ " - " ++ defaultTitle))
      else ({model | secsLeft = Nothing,
                     tPomodoroEnd = Nothing,
                     completedPomodoros=model.completedPomodoros + 1},
            Cmd.batch [External.title defaultTitle,
                       Audio.playSound "ring.ogg",
                       maybeRestoreStatus model,
                       maybeSetPresence model Auto])
    Nothing -> (model, External.title defaultTitle)

parseCookies : String -> Dict String String
parseCookies s =
  let l     = String.split ";" s
      parts = List.map (\ps -> let pair = String.split "=" ps in (withDefault "" <| head pair, withDefault "" <| head (withDefault [] <| tail pair))) l
  in Dict.fromList parts

handleCookies : String -> Model -> Model
handleCookies s model =
  let cookies = parseCookies s
  in if Dict.member "token" cookies
     then {model | authToken = Just <| Maybe.withDefault "" (Dict.get "token" cookies)}
     else model