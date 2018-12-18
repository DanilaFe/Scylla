module Scylla.Http exposing (..)
import Scylla.Model exposing (..)
import Scylla.Api exposing (..)
import Scylla.Route exposing (RoomId)
import Scylla.Sync exposing (syncResponseDecoder)
import Scylla.Login exposing (loginResponseDecoder, Username, Password)
import Scylla.UserData exposing (userDataDecoder, UserData)
import Json.Encode exposing (object, string, int, bool)
import Http exposing (request, emptyBody, jsonBody, expectJson, expectWhatever)

fullClientUrl : ApiUrl -> ApiUrl
fullClientUrl s = s ++ "/_matrix/client/r0"

fullMediaUrl : ApiUrl -> ApiUrl
fullMediaUrl s = s ++ "/_matrix/media/r0"

-- Http Requests
firstSync : ApiUrl -> ApiToken -> Cmd Msg
firstSync apiUrl token = request
    { method = "GET"
    , headers = authenticatedHeaders token
    , url = (fullClientUrl apiUrl) ++ "/sync"
    , body = emptyBody
    , expect = expectJson ReceiveFirstSyncResponse syncResponseDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

sync : String -> ApiUrl -> ApiToken -> Cmd Msg
sync nextBatch apiUrl token = request
    { method = "GET"
    , headers = authenticatedHeaders token
    , url = (fullClientUrl apiUrl) ++ "/sync" ++ "?since=" ++ (nextBatch) ++ "&timeout=10000"
    , body = emptyBody
    , expect = expectJson ReceiveSyncResponse syncResponseDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

sendTextMessage : ApiUrl -> ApiToken -> Int -> String -> String -> Cmd Msg
sendTextMessage apiUrl token transactionId room message = request
    { method = "PUT"
    , headers = authenticatedHeaders token
    , url = (fullClientUrl apiUrl)
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
    , url = (fullClientUrl apiUrl) ++ "/login"
    , body = jsonBody <| object
        [ ("type", string "m.login.password")
        , ("identifier", object
            [ ("type", string "m.id.user")
            , ("user", string username)
            ] )
        , ("password", string password)
        ]
    , expect = expectJson (ReceiveLoginResponse apiUrl) loginResponseDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

userData : ApiUrl -> ApiToken -> Username -> Cmd Msg
userData apiUrl token username = request
    { method = "GET"
    , headers = authenticatedHeaders token
    , url = (fullClientUrl apiUrl) ++ "/profile/" ++ username
    , body = emptyBody
    , expect = expectJson (ReceiveUserData username) userDataDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

setReadMarkers : ApiUrl -> ApiToken -> String -> RoomId -> Maybe String -> Cmd Msg
setReadMarkers apiUrl token roomId fullyRead readReceipt =
    let
        readReciptFields = case readReceipt of
            Just s -> [ ("m.read", string s) ]
            _ -> []
    in
        request
            { method = "POST"
            , headers = authenticatedHeaders token
            , url = (fullClientUrl apiUrl) ++ "/rooms/" ++ roomId ++ "/read_markers"
            , body = jsonBody <| object <| [ ("m.fully_read", string fullyRead) ] ++ readReciptFields
            , expect = expectWhatever ReceiveCompletedReadMarker
            , timeout = Nothing
            , tracker = Nothing
            }

sendTypingIndicator : ApiUrl -> ApiToken -> RoomId -> Username -> Bool -> Int -> Cmd Msg
sendTypingIndicator apiUrl token room user isTyping timeout = request
    { method = "PUT"
    , headers = authenticatedHeaders token
    , url = (fullClientUrl apiUrl) ++ "/rooms/" ++ room ++ "/typing/" ++ user
    , body = jsonBody <| object [ ("timeout", int timeout), ("typing", bool isTyping) ]
    , expect = expectWhatever ReceiveCompletedTypingIndicator
    , timeout = Nothing
    , tracker = Nothing
    }
