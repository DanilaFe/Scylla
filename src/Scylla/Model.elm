module Scylla.Model exposing (..)
import Scylla.Api exposing (..)
import Scylla.Sync exposing (SyncResponse, JoinedRoom, senderName)
import Scylla.Login exposing (LoginResponse, Username, Password)
import Scylla.UserData exposing (UserData)
import Scylla.Route exposing (Route(..), RoomId)
import Scylla.Storage exposing (..)
import Browser.Navigation as Nav
import Browser.Dom exposing (Viewport)
import Url.Builder
import Dict exposing (Dict)
import Json.Decode
import Browser
import Http
import Url exposing (Url)

type alias Model =
    { key : Nav.Key
    , route : Route
    , token : Maybe ApiToken
    , loginUsername : Username
    , loginPassword : Password
    , apiUrl : ApiUrl
    , sync : SyncResponse
    , errors : List String
    , roomText : Dict String String
    , transactionId : Int
    , userData : Dict Username UserData
    }

type Msg =
    ChangeApiUrl ApiUrl -- During login screen: the API URL (homeserver)
    | ChangeLoginUsername Username -- During login screen: the username
    | ChangeLoginPassword Password -- During login screen: the password
    | AttemptLogin -- During login screen, login button presed
    | TryUrl Browser.UrlRequest -- User attempts to change URL
    | OpenRoom String -- We try open a room
    | ChangeRoute Route -- URL changes
    | ChangeRoomText String String -- Change to a room's input text
    | SendRoomText String -- Sends a message typed into a given room's input
    | SendRoomTextResponse (Result Http.Error ()) -- A send message response finished
    | ViewportAfterMessage (Result Browser.Dom.Error Viewport) -- A message has been received, try scroll (maybe)
    | ViewportChangeComplete (Result Browser.Dom.Error ()) -- We're done changing the viewport.
    | ReceiveFirstSyncResponse (Result Http.Error SyncResponse) -- HTTP, Sync has finished
    | ReceiveSyncResponse (Result Http.Error SyncResponse) -- HTTP, Sync has finished
    | ReceiveLoginResponse ApiUrl (Result Http.Error LoginResponse) -- HTTP, Login has finished
    | ReceiveUserData Username (Result Http.Error UserData)
    | ReceiveCompletedReadMarker (Result Http.Error ())
    | ReceiveStoreData Json.Decode.Value
    | ReceiveCompletedTypingIndicator (Result Http.Error ()) -- HTTP, typing indicator request completed

displayName : Model -> Username -> String
displayName m s = Maybe.withDefault (senderName s) <| Maybe.andThen .displayName <| Dict.get s m.userData

roomUrl : String -> String
roomUrl s = Url.Builder.absolute [ "room", s ] []

loginUrl : String
loginUrl = Url.Builder.absolute [ "login" ] []

currentRoom : Model -> Maybe JoinedRoom
currentRoom m =
    let
        roomDict = Maybe.withDefault Dict.empty <| Maybe.andThen .join <| m.sync.rooms
    in
        Maybe.andThen (\s -> Dict.get s roomDict) <| currentRoomId m

currentRoomId : Model -> Maybe RoomId
currentRoomId m = case m.route of
    Room r -> Just r
    _ -> Nothing
