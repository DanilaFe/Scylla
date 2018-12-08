import Browser exposing (application)
import Browser.Navigation as Nav
import Scylla.Sync exposing (..)
import Scylla.Login exposing (..)
import Scylla.Model exposing (..)
import Scylla.Http exposing (..)
import Scylla.Views exposing (viewFull)
import Url exposing (Url)
import Html exposing (div, text)
import Http

type alias Flags =
    { token : Maybe String
    }

init : Flags -> Url -> Nav.Key -> (Model, Cmd Msg)
init flags url key =
    let
        model =
            { key = key
            , token = flags.token
            , loginUsername = ""
            , loginPassword = ""
            , apiUrl = "https://matrix.org"
            }
        cmd = case flags.token of
            Just _ -> Cmd.none
            Nothing -> Cmd.none
    in
        (model, cmd)

view : Model -> Browser.Document Msg
view m =
    { title = "Scylla"
    , body = [ viewFull m ]
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    ChangeApiUrl u -> ({ model | apiUrl = u }, Cmd.none)
    ChangeLoginUsername u -> ({ model | loginUsername = u }, Cmd.none)
    ChangeLoginPassword p -> ({ model | loginPassword = p }, Cmd.none)
    AttemptLogin -> (model, Scylla.Http.login model.apiUrl model.loginUsername model.loginPassword) -- TODO 
    ReceiveLoginResponse r -> updateLoginResponse model r
    ReceiveSyncResponse r -> updateSyncResponse model r
    _ -> (model, Cmd.none)

updateLoginResponse : Model -> Result Http.Error LoginResponse -> (Model, Cmd Msg)
updateLoginResponse model r = case r of
    Ok lr -> ( { model | token = Just lr.accessToken } , firstSync model.apiUrl lr.accessToken )
    Err e  -> (model, Cmd.none)

updateSyncResponse : Model -> Result Http.Error SyncResponse -> (Model, Cmd Msg)
updateSyncResponse model r = case r of
    Ok sr -> (model, Cmd.none)
    Err e -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions m = Sub.none

onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest = TryUrl

onUrlChange : Url -> Msg
onUrlChange = ChangeUrl

main = application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = onUrlRequest
    , onUrlChange = onUrlChange
    }
