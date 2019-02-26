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
            , sendingMessages = []
            , inputText = Nothing
            }
        Nothing -> Nothing

currentRoomData : Model -> Maybe RoomData
currentRoomData m = Maybe.andThen (roomData m) <| currentRoomId m
