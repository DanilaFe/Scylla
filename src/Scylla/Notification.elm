port module Scylla.Notification exposing (..)
import Scylla.Sync exposing (SyncResponse, RoomEvent, joinedRoomsTimelineEvents)
import Scylla.AccountData exposing (..)
import Json.Decode as Decode exposing (string, field)
import Dict

type alias Notification =
    { name : String
    , text : String
    , room : String
    }

port sendNotificationPort : Notification -> Cmd msg
port onNotificationClickPort : (String -> msg) -> Sub msg

notificationText : RoomEvent -> String
notificationText re = case (Decode.decodeValue (field "msgtype" string) re.content) of
    Ok "m.text" -> Result.withDefault "" <| (Decode.decodeValue (field "body" string) re.content)
    _ -> ""

joinedRoomNotificationEvents : SyncResponse -> List (String, RoomEvent)
joinedRoomNotificationEvents s =
    let
        applyPair k = List.map (\v -> (k, v))
    in
        List.sortBy (\(k, v) -> v.originServerTs)
        <| Dict.foldl (\k v a -> a ++ applyPair k v) []
        <| joinedRoomsTimelineEvents s

