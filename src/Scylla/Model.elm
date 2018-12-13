module Scylla.Model exposing (..)
import Scylla.Api exposing (..)
import Scylla.Sync exposing (SyncResponse, JoinedRoom)
import Scylla.Login exposing (LoginResponse, Username, Password)
import Scylla.UserData exposing (UserData)
import Scylla.Route exposing (Route)
import Browser.Navigation as Nav
import Dict exposing (Dict)
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
    | ChangeRoute Route -- URL changes
    | ChangeRoomText String String -- Change to a room's input text
    | SendRoomText String -- Sends a message typed into a given room's input
    | SendRoomTextResponse (Result Http.Error ()) -- A send message response finished
    | ReceiveFirstSyncResponse (Result Http.Error SyncResponse) -- HTTP, Sync has finished
    | ReceiveSyncResponse (Result Http.Error SyncResponse) -- HTTP, Sync has finished
    | ReceiveLoginResponse (Result Http.Error LoginResponse) -- HTTP, Login has finished
    | ReceiveUserData Username (Result Http.Error UserData)

