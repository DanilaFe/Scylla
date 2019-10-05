port module Scylla.Notification exposing (..)
import Scylla.Sync exposing (SyncResponse, joinedRoomsTimelineEvents)
import Scylla.Sync.Events exposing (RoomEvent, MessageEvent, toMessageEvent)
import Scylla.Sync.Push exposing (Ruleset, getEventNotification)
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

getNotificationEvents : Ruleset -> SyncResponse -> List (String, MessageEvent)
getNotificationEvents rs s = s.rooms
    |> Maybe.andThen .join
    |> Maybe.map (Dict.map (\k v -> v.timeline
        |> Maybe.andThen .events
        |> Maybe.map (List.filter <| getEventNotification rs k)
        |> Maybe.map (List.filterMap <| toMessageEvent)
        |> Maybe.withDefault []))
    |> Maybe.withDefault Dict.empty
    |> Dict.toList
    |> List.concatMap (\(k, vs) -> List.map (\v -> (k, v)) vs)
