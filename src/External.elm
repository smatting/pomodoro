port module External exposing (..)

port getCurrentUrl : () -> Cmd msg
port currentUrl : (String -> msg) -> Sub msg

port title : String -> Cmd a


