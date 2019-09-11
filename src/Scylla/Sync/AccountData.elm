module Scylla.Sync.AccountData exposing (..)
import Scylla.Sync.DecodeTools exposing (maybeDecode)
import Scylla.Sync.Events exposing (Event, eventDecoder)
import Json.Decode as Decode exposing (Decoder, list)

type alias AccountData =
    { events : Maybe (List Event)
    }

accountDataDecoder : Decoder AccountData
accountDataDecoder =
    Decode.succeed AccountData
        |> maybeDecode "events" (list eventDecoder)

