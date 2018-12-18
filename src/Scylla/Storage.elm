port module Scylla.Storage exposing (..)
import Json.Encode
import Json.Decode as Decode exposing (Decoder, int, string, float, list, value, dict, bool)
import Json.Decode.Pipeline exposing (required, optional)

type alias StoreData =
    { key : String
    , value: Decode.Value
    }

storeDataDecoder : Decoder StoreData
storeDataDecoder = Decode.succeed StoreData
    |> required "key" string
    |> required "value" value

port setStoreValuePort : (String, Json.Encode.Value) -> Cmd msg
port getStoreValuePort : (String) -> Cmd msg
port receiveStoreValuePort : (Json.Encode.Value -> msg) -> Sub msg
