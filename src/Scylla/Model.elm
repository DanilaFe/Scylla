module Scylla.Model exposing (..)
import Scylla.Api exposing (..)
import Scylla.Sync exposing (SyncResponse)
import Scylla.Login exposing (LoginResponse)
import Browser.Navigation as Nav
import Browser
import Http
import Url exposing (Url)

type alias Model =
    { key : Nav.Key
    , token : Maybe ApiToken
    , apiUrl : ApiUrl
    }

type Msg =
    TryUrl Browser.UrlRequest
    | ChangeUrl Url
    | ReceiveSyncResponse (Result Http.Error SyncResponse)
    | ReceiveLoginResponse (Result Http.Error LoginResponse)

