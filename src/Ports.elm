port module Ports exposing (..)

import Json.Encode exposing (Value)

port scrollToVisible : String -> Cmd a

port load : () -> Cmd a

port loadedData : (Value -> msg) -> Sub msg

port save : String -> Cmd a
