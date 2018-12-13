module Scylla.UserData exposing (..)
import Json.Decode as Decode exposing (Decoder, int, string, float, list, value, dict, bool, field)
import Json.Decode.Pipeline exposing (required, optional)

type alias UserData =
    { displayName : Maybe String
    , avatarUrl : Maybe String
    }

userDataDecoder : Decoder UserData
userDataDecoder =
    Decode.succeed UserData
        |> optional "displayname" (Decode.map Just string) Nothing
        |> optional "avatar_url" (Decode.map Just string) Nothing
