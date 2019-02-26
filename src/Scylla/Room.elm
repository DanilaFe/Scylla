module Scylla.Room exposing (..)
import Scylla.Model exposing (..)
import Scylla.Sync exposing (..)
import Scylla.Messages exposing (..)
import Scylla.Route exposing (..)
import Dict

type alias RoomData =
    { joinedRoom : JoinedRoom
    , sendingMessages : List (SendingMessage, Int)
    , inputText : Maybe String
    }

roomData : Model -> RoomId -> Maybe RoomData
roomData m rid =
    case Dict.get rid (joinedRooms m) of
        Just jr -> Just
            { joinedRoom = jr
            , sendingMessages = List.map (\(tid, (_, sm)) -> (sm, tid)) <| List.filter (\(_, (nrid, _)) -> nrid == rid) <| Dict.toList m.sending
            , inputText = Dict.get rid m.roomText
            }
        Nothing -> Nothing

currentRoomData : Model -> Maybe RoomData
currentRoomData m = Maybe.andThen (roomData m) <| currentRoomId m

extractMessageEvents : List RoomEvent -> List Message
extractMessageEvents es = List.map Received
    <| List.filter (\e -> e.type_ == "m.room.message") es

extractMessages : RoomData -> List Message
extractMessages rd =
    let
        eventMessages = extractMessageEvents <| Maybe.withDefault [] <| Maybe.andThen .events rd.joinedRoom.timeline
        sendingMessages = List.map (\(sm, i) -> Sending sm) rd.sendingMessages
    in
        eventMessages ++ sendingMessages
