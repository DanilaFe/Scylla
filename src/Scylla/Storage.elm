port module Scylla.Storage exposing (..)
import Json.Encode

type alias StoreData =
    { key : String
    , value: String
    }

port setStoreValuePort : (String, Json.Encode.Value) -> Cmd msg
port getStoreValuePort : (String) -> Cmd msg
port receiveStoreValuePort : (StoreData -> msg) -> Sub msg
