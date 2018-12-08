module Scylla.Model exposing (..)
import Scylla.Api exposing (..)
import Scylla.Sync exposing (SyncResponse)
import Scylla.Login exposing (LoginResponse, Username, Password)
import Browser.Navigation as Nav
import Browser
import Http
import Url exposing (Url)

type alias Model =
    { key : Nav.Key
    , token : Maybe ApiToken
    , loginUsername : Username
    , loginPassword : Password
    , apiUrl : ApiUrl
    }

type Msg =
    ChangeApiUrl ApiUrl -- During login screen: the API URL (homeserver)
    | ChangeLoginUsername Username -- During login screen: the username
    | ChangeLoginPassword Password -- During login screen: the password
    | AttemptLogin -- During login screen, login button presed
    | TryUrl Browser.UrlRequest -- User attempts to change URL
    | ChangeUrl Url -- URL changes
    | ReceiveSyncResponse (Result Http.Error SyncResponse) -- HTTP, Sync has finished
    | ReceiveLoginResponse (Result Http.Error LoginResponse) -- HTTP, Login has finished

