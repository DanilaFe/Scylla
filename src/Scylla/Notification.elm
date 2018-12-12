port module Scylla.Notification exposing (..)
import Scylla.Model exposing (..)
import Json.Decode

type alias Notification =
    { name : String
    , text : String
    }

port sendNotificationPort : Notification -> Cmd msg
port onNotificationClockPort : (Json.Decode.Value -> msg) -> Sub msg
