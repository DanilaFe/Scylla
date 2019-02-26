module Scylla.Messages exposing (..)
import Scylla.Model exposing (Model)
import Scylla.Sync exposing (RoomEvent)
import Scylla.Login exposing (Username)

type SendingMessage = TextMessage String

type Message =
    Sending SendingMessage
    | Received RoomEvent

extractMessageEvents : List RoomEvent -> List Message
extractMessageEvents es = List.map Received
    <| List.filter (\e -> e.type_ == "m.room.message") es

messageUsername : Model -> Message -> Username
messageUsername m msg = case msg of
    Sending _ -> m.loginUsername
    Received re -> re.sender

mergeMessages : Model -> List Message -> List (Username, List Message)
mergeMessages m xs =
    let
        initialState = (Nothing, [], [])
        appendNamed mu ms msl = case mu of
            Just u -> msl ++ [(u, ms)]
            Nothing -> msl
        foldFunction msg (pu, ms, msl) =
            let
                nu = Just <| messageUsername m msg
            in
                if pu == nu then (pu, ms ++ [msg], msl) else (nu, [msg], appendNamed pu ms msl)
        (fmu, fms, fmsl) = List.foldl foldFunction initialState xs
    in
        appendNamed fmu fms fmsl
