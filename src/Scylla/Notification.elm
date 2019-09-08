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

producesNotification : NotificationSetting -> RoomEvent -> Bool
producesNotification ns re = case ns of
    Normal -> True
    _ -> False

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
        <| Dict.map (\k v -> List.filter (producesNotification (roomIdNotificationSetting s k)) v)
        <| joinedRoomsTimelineEvents s

