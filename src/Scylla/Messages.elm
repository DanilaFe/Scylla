module Scylla.Messages exposing (..)
import Scylla.Sync exposing (RoomEvent)
import Scylla.Login exposing (Username)

type SendingMessageBody = TextMessage String

type alias SendingMessage =
    { body : SendingMessageBody
    , id : Maybe String
    }

type Message =
    Sending SendingMessage
    | Received RoomEvent

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
