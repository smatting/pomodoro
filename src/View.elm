module View exposing (..)

import Html exposing (Html, button, div, text, program, span, a, img, iframe)
import Html.Events exposing (onClick, on, onWithOptions, onInput)
import Html.Attributes as H exposing (..)
import Svg
import Svg.Attributes as S exposing (..)
import Path.LowLevel as LL exposing (Coordinate)
import Maybe exposing (withDefault)

import Common exposing (..)

svgArc : Bool ->  Float -> Html Msg
svgArc running clock =
  let
    radius    = 240
    rad       = -2.0 * pi * (clock / 12.0 + 0.75)
    x         = radius * cos rad
    y         = radius * -1.0 * sin rad + radius
    arcFlag   = if clock < 6.0 then LL.SmallestArc else LL.LargestArc
    direction = LL.CounterClockwise
    pathSpec  = LL.toString [{moveto = LL.MoveTo LL.Absolute (250, 10),
                              drawtos = [LL.EllipticalArc
                                         LL.Relative
                                         [{radii = (radius, radius),
                                           xAxisRotate = 0,
                                           arcFlag = arcFlag,
                                           direction = direction,
                                           target = (x, y)}]]}]
  in Svg.svg
     [S.id "progress-circle",
      S.width "500",
      S.height "500",
      S.viewBox "0 0 500 500",
      S.class (if running then "running" else "notrunning") ]
     [Svg.path [d pathSpec,
                stroke "rgba(200,0,0,0.6)",
                fill "none",
                strokeWidth "20",
                strokeLinecap "round"] []]

view : Model -> Html Msg
view model =
  let progress = Maybe.withDefault 0.9999 (Maybe.map (\s -> 1 - s / pomodoroLength) model.secsLeft)
  in
  div [H.class "content"]
      [
--       div [H.class "debug-window"]
--            [div [] [text "Backup Status"],
--             div [] [text "text: ",
--                     text (maybe "(Nothing)" (\status -> status.text) model.backupStatus)],
--             div [] [text "emoji: ",
--                     text (maybe "(Nothing)" (\status -> status.emoji) model.backupStatus)],
--             div [] [text "Slack Status"],
--             div [] [text "text: ",
--                     text (maybe "(Nothing)" (\status -> status.text) model.status)],
--             div [] [text "emoji: ",
--                     text (maybe "(Nothing)" (\status -> status.emoji) model.status)],
--             div [] [text "set presence: ", text (toString model.setPresence)],
--             div [] [text "presence: ", text (toString model.presence)],
--             div [] [text "cookies: ", text <| toString <| model.cookies],
--             div [] [text "authToken: ", text <| toString <| model.authToken]
--             ],
        
       githubButton,
       if (isJust model.authToken) then (span [] []) else slackButton,

       div ((if isPomodoroRunning(model)
             then []
             else [onClick StartPomodoro])
            ++ [H.classList [("pomodoro", True),
                             ("notrunning", not (isPomodoroRunning model))]])
           [svgArc (isPomodoroRunning model) (progress * 12),
            div [H.classList [("timer", True),
                              ("unselectable", True),
                              ("notrunning", not (isPomodoroRunning model))]]
                [(text (formatDuration (withDefault pomodoroLength model.secsLeft)))]],

        div [H.classList [("buttons-lower", True),
                          ("notrunning", not (isPomodoroRunning model))]]
            [button [H.class "pure-button cancel-button", onClick ResetPomodoro]
                    [Html.i [H.class "fa fa-cog"] [],
                     text "Cancel Pomodoro" ]],

        div [H.class "completed-pomodoros"] 
            (List.map (\i -> span [H.class "completed-pomodoro"] [])
                      (List.range 1 model.completedPomodoros))]

slackButton : Html Msg
slackButton =
  a [H.href "https://slack.com/oauth/authorize?client_id=6579745568.340832914387&scope=users.profile:write,users.profile:read,users:write", H.class "slack-button"] [
    img [H.alt "Add to Slack", H.height 40, H.width 139, H.src "https://platform.slack-edge.com/img/add_to_slack.png"] []
  ]  

githubButton : Html Msg
githubButton =
  iframe [H.src "https://ghbtns.com/github-btn.html?user=smatting&repo=pomodoro&type=star&count=true&size=large",
          H.attribute "frameborder" "0",
          H.attribute "scrolling" "0",
          H.class "github-button",
          H.width 160,
          H.height 30]
          []