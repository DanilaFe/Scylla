port module Scylla.Notification exposing (..)
import Scylla.Sync exposing (SyncResponse, joinedRoomsTimelineEvents)
import Scylla.Sync.Events exposing (RoomEvent, MessageEvent, toMessageEvent)
import Json.Decode as Decode exposing (string, field)
import Dict

type alias Notification =
    { name : String
    , text : String
    , room : String
    }

port sendNotificationPort : Notification -> Cmd msg
port onNotificationClickPort : (String -> msg) -> Sub msg

getText : MessageEvent -> String
getText re = case (Decode.decodeValue (field "msgtype" string) re.content) of
    Ok "m.text" -> Result.withDefault "" <| (Decode.decodeValue (field "body" string) re.content)
    _ -> ""

getNotificationEvents : SyncResponse -> List (String, MessageEvent)
getNotificationEvents s =
    let
        applyPair k = List.map (\v -> (k, v))
    in
        List.sortBy (\(k, v) -> v.originServerTs)
        <| List.filterMap (\(k, e) -> Maybe.map (\me -> (k, me)) <| toMessageEvent e)
        <| Dict.foldl (\k v a -> a ++ applyPair k v) []
        <| joinedRoomsTimelineEvents s

