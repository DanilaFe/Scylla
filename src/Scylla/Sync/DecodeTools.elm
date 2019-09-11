module Scylla.Sync.DecodeTools exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional)

decodeJust : Decoder a -> Decoder (Maybe a)
decodeJust = Decode.map Just

maybeDecode : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
maybeDecode s d = optional s (decodeJust d) Nothing
