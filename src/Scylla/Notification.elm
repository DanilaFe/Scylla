port module Scylla.Notification exposing (..)
import Json.Decode

type alias Notification =
    { name : String
    , text : String
    , room : String
    }

port sendNotificationPort : Notification -> Cmd msg
port onNotificationClickPort : (String -> msg) -> Sub msg
