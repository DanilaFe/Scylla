module Scylla.Login exposing (..)
import Scylla.Api exposing (ApiUrl, ApiToken)
import Json.Decode as Decode exposing (Decoder, int, string, float, list, value, dict, bool)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode

type alias Username = String
type alias Password = String

type alias LoginInfo =
    { token : ApiToken
    , apiUrl : ApiUrl
    , username : Username
    , transactionId : Int
    }

encodeLoginInfo : LoginInfo -> String
encodeLoginInfo {token, apiUrl, username, transactionId} =
    token ++ "," ++ apiUrl ++ "," ++ username ++ "," ++ (String.fromInt transactionId)

decodeLoginInfo : String -> Maybe LoginInfo
decodeLoginInfo s = case String.indexes "," s of
    [ fst, snd, thd ] -> Just <| LoginInfo
        (String.slice 0 fst s)
        (String.slice (fst + 1) snd s)
        (String.slice (snd + 1) thd s)
        (Maybe.withDefault 0 <| String.toInt <| String.dropLeft (thd + 1) s)
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
