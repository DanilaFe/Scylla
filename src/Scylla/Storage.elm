port module Scylla.Storage exposing (..)
import Json.Encode

port setStoreValuePort : (String, Json.Encode.Value) -> Cmd msg
port getStoreValuePort : (String) -> Cmd msg
port receiveStoreValuePort : (Json.Encode.Value -> msg) -> Sub msg
