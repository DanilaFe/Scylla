module Scylla.AccountData exposing (..)
import Scylla.Sync exposing (SyncResponse, roomAccountData)
import Scylla.Sync.AccountData exposing (AccountData)
import Scylla.Sync.Rooms exposing (JoinedRoom)
import Json.Decode as Decode
import Json.Encode as Encode
import Dict exposing (Dict)

type alias DirectMessages = Dict String String
type alias DirectMessagesRaw = Dict String (List String)

directMessagesDecoder : Decode.Decoder DirectMessages
directMessagesDecoder =
    Decode.dict (Decode.list Decode.string)
        |> Decode.map (invertDirectMessages)

invertDirectMessages : DirectMessagesRaw -> DirectMessages
invertDirectMessages dmr =
    Dict.foldl
        (\k lv acc -> List.foldl (\v -> Dict.insert v k) acc lv)
        Dict.empty
        dmr

