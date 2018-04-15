module Common exposing (..)
import Time exposing (Time, now)

type alias Model = {
    tPomodoroEnd : Maybe Time,
    secsLeft : Maybe Float,
    completedPomodoros : Int,
    showSettings : Bool,
    authToken : Maybe String,
    backupStatus : Maybe Status,
    status : Maybe Status
}

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

type alias Status = {
    text: String,
    emoji: String,
    expiration: Int
}


pomodoroLength : Float
pomodoroLength = 25 * Time.minute
-- pomodoroLength = 2 * Time.second

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


formatMillis : Float -> String
formatMillis dt =
    let n = ceiling (dt / 1000.0)
        minutes = n // 60
        seconds = n % 60
    in (format2digits minutes) ++ ":" ++ (format2digits seconds)

format2digits : Int -> String
format2digits n =
    if n < 10 then "0" ++ (toString n)
    else toString n 
