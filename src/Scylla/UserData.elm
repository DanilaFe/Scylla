module Scylla.UserData exposing (..)
import Scylla.Login exposing (Username)
import Json.Decode as Decode exposing (Decoder, int, string, float, list, value, dict, bool, field)
import Json.Decode.Pipeline exposing (required, optional)
import Dict exposing (Dict)

type alias UserData =
    { displayName : Maybe String
    , avatarUrl : Maybe String
    }

userDataDecoder : Decoder UserData
userDataDecoder =
    Decode.succeed UserData
        |> optional "displayname" (Decode.map Just string) Nothing
        |> optional "avatar_url" (Decode.map Just string) Nothing

getDisplayName : Dict Username UserData -> Username -> String
getDisplayName ud s = Dict.get s ud
    |> Maybe.andThen .displayName 
    |> Maybe.withDefault (getSenderName s) 

getSenderName : Username -> String
getSenderName s =
    let
        colonIndex = Maybe.withDefault -1 
            <| List.head
            <| String.indexes ":" s
    in
        String.slice 1 colonIndex s

