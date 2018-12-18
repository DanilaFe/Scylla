module Scylla.Login exposing (..)
import Scylla.Api exposing (ApiUrl, ApiToken)
import Json.Decode as Decode exposing (Decoder, int, string, float, list, value, dict, bool)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode

type alias Username = String
type alias Password = String

type alias LoginInfo = (ApiToken, ApiUrl, Username)

encodeLoginInfo : LoginInfo -> String
encodeLoginInfo (t,a,u) = t ++ "," ++ a ++ "," ++ u

decodeLoginInfo : String -> Maybe LoginInfo
decodeLoginInfo s = case String.indexes "," s of
    [ fst, snd ] -> Just
        ( (String.slice 0 fst s)
        , (String.slice (fst + 1) snd s)
        , (String.dropLeft (snd + 1) s)
        )
    _ -> Nothing

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
