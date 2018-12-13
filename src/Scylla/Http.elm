module Scylla.Http exposing (..)
import Scylla.Model exposing (..)
import Scylla.Api exposing (..)
import Scylla.Sync exposing (syncResponseDecoder)
import Scylla.Login exposing (loginResponseDecoder, Username, Password)
import Scylla.UserData exposing (userDataDecoder, UserData)
import Json.Encode exposing (object, string, int)
import Http exposing (request, emptyBody, jsonBody, expectJson, expectWhatever)

fullUrl : ApiUrl -> ApiUrl
fullUrl s = s ++ "/_matrix/client/r0"

-- Http Requests
firstSync : ApiUrl -> ApiToken -> Cmd Msg
firstSync apiUrl token = request
    { method = "GET"
    , headers = authenticatedHeaders token
    , url = (fullUrl apiUrl) ++ "/sync"
    , body = emptyBody
    , expect = expectJson ReceiveFirstSyncResponse syncResponseDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

sync : String -> ApiUrl -> ApiToken -> Cmd Msg
sync nextBatch apiUrl token = request
    { method = "GET"
    , headers = authenticatedHeaders token
    , url = (fullUrl apiUrl) ++ "/sync" ++ "?since=" ++ (nextBatch) ++ "&timeout=10000"
    , body = emptyBody
    , expect = expectJson ReceiveSyncResponse syncResponseDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

sendTextMessage : ApiUrl -> ApiToken -> Int -> String -> String -> Cmd Msg
sendTextMessage apiUrl token transactionId room message = request
    { method = "PUT"
    , headers = authenticatedHeaders token
    , url = (fullUrl apiUrl)
        ++ "/rooms/" ++ room
        ++ "/send/" ++ "m.room.message"
        ++ "/" ++ (String.fromInt transactionId)
    , body = jsonBody <| object
        [ ("msgtype", string "m.text")
        , ("body", string message)
        ]
    , expect = expectWhatever SendRoomTextResponse
    , timeout = Nothing
    , tracker = Nothing
    }

login : ApiUrl -> Username -> Password -> Cmd Msg
login apiUrl username password = request
    { method = "POST"
    , headers = basicHeaders
    , url = (fullUrl apiUrl) ++ "/login"
    , body = jsonBody <| object
        [ ("type", string "m.login.password")
        , ("identifier", object
            [ ("type", string "m.id.user")
            , ("user", string username)
            ] )
        , ("password", string password)
        ]
    , expect = expectJson ReceiveLoginResponse loginResponseDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

userData : ApiUrl -> ApiToken -> Username -> Cmd Msg
userData apiUrl token username = request
    { method = "GET"
    , headers = authenticatedHeaders token
    , url = (fullUrl apiUrl) ++ "/profile/" ++ username
    , body = emptyBody
    , expect = expectJson (ReceiveUserData username) userDataDecoder
    , timeout = Nothing
    , tracker = Nothing
    }
