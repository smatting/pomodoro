module Main exposing (..)
import Html exposing (Html, button, div, text, program, span)
import Time exposing (Time, now)
import Maybe exposing (withDefault)
import External

import View exposing (view)
import Control exposing (init, update)
import Common exposing (..)

subscriptions : Model -> Sub Msg
subscriptions model =
  let listenUrl = External.currentUrl (\s -> UrlUpdate s)
  in case model.tPomodoroEnd of
    Just t  -> Sub.batch [ Time.every (0.1 * Time.second) Tick, listenUrl ]
    Nothing -> listenUrl 

main : Program Never Model Msg
main =
  program {init   = init,
           view   = view,
           update = update,
           subscriptions = subscriptions}
