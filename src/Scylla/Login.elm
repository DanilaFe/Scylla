module Scylla.Login exposing (..)
import Scylla.Api exposing (ApiToken)
import Json.Decode as Decode exposing (Decoder, int, string, float, list, value, dict, bool)
import Json.Decode.Pipeline exposing (required, optional)

type alias Username = String
type alias Password = String

type alias LoginResponse =
    { userId : String
    , accessToken : ApiToken
    , deviceId : String
    }

loginResponseDecoder : Decoder LoginResponse
loginResponseDecoder =
    Decode.succeed LoginResponse
        |> required "user_id" string
        |> required "access_token" string
        |> required "device_id" string
