module Common exposing (..)
import Time exposing (Time, now)
import Strftime exposing (format)
import Date

type alias Model = {
    tPomodoroEnd : Maybe Time,
    secsLeft : Maybe Float,
    completedPomodoros : Int,
    showSettings : Bool,
    authToken : Maybe String,
    backupStatus : Maybe Status,
    status : Maybe Status,
    setPresence : Bool,
    presence : Maybe Presence,
    cookies : Maybe String
}

type Msg
    = SetTPomodoro Time Float
    | StartPomodoro
    | ResetPomodoro
    | Tick Time
    | ToggleSettings
    | UrlUpdate String
    | StatusUpdate (Maybe Status) (Maybe Status)
    | PresenceUpdate Presence
    | CookieUpdate String
    | Void     

type alias Status = {
    text: String,
    emoji: String,
    expiration: Int
}

type Presence = Away | Auto

pomodoroLength : Float
-- pomodoroLength = 7 * Time.second
pomodoroLength = 25 * Time.minute

isPomodoroRunning : Model -> Bool
isPomodoroRunning model =
    isJust model.tPomodoroEnd

isJust : Maybe a -> Bool
isJust x =
    case x of
    Just _ -> True
    Nothing -> False

maybe : b -> (a -> b) -> Maybe a -> b
maybe default f m =
    case m of 
        Just v -> f v
        Nothing -> default

format2digits : Int -> String -> String
format2digits n filling =
    if n < 10 then filling ++ (toString n)
    else toString n 

formatDuration : Float -> String
formatDuration dt =
    let secsTotal = ceiling (dt / 1000.0)
        minutes   = secsTotal // 60
        seconds   = secsTotal % 60
    in (format2digits minutes " ") ++ ":" ++ (format2digits seconds "0")

formatDurationMinutes : Float -> String
formatDurationMinutes dt =
    let secsTotal = ceiling (dt / 1000.0)
        minutes   = secsTotal // 60
        seconds   = secsTotal % 60
        mins      = minutes + (if seconds > 10 then 1 else 0)
    in toString mins ++ "m"

formatTime : Time -> String
formatTime time =
  let date = Date.fromTime time
  in format "%H:%M" date