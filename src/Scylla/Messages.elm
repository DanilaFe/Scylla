module Scylla.Messages exposing (..)
import Scylla.Sync.Events exposing (RoomEvent, MessageEvent, toMessageEvent)
import Scylla.Login exposing (Username)
import Scylla.Route exposing (RoomId)
import Dict exposing (Dict)

type SendingMessageBody = TextMessage String

type alias SendingMessage =
    { body : SendingMessageBody
    , id : Maybe String
    }

type Message
    = Sending SendingMessage
    | Received MessageEvent

messageUsername : Username -> Message -> Username
messageUsername u msg = case msg of
    Sending _ -> u
    Received re -> re.sender

mergeMessages : Username -> List Message -> List (Username, List Message)
mergeMessages du xs =
    let
        initialState = (Nothing, [], [])
        appendNamed mu ms msl = case mu of
            Just u -> msl ++ [(u, ms)]
            Nothing -> msl
        foldFunction msg (pu, ms, msl) =
            let
                nu = Just <| messageUsername du msg
            in
                if pu == nu then (pu, ms ++ [msg], msl) else (nu, [msg], appendNamed pu ms msl)
        (fmu, fms, fmsl) = List.foldl foldFunction initialState xs
    in
        appendNamed fmu fms fmsl

receivedMessagesRoom : List RoomEvent -> List Message
receivedMessagesRoom es = List.map Received
    <| List.filter (\e -> e.type_ == "m.room.message")
    <| List.filterMap toMessageEvent es

sendingMessagesRoom : RoomId -> Dict Int (RoomId, SendingMessage) -> List Message
sendingMessagesRoom rid ms = List.map (\(tid, (_, sm)) -> Sending sm)
    <| List.filter (\(_, (nrid, _)) -> nrid == rid)
    <| Dict.toList ms
