import Browser exposing (application)
import Browser.Navigation as Nav
import Url exposing (Url)
import Html exposing (div)

type alias Flags =
    { token : Maybe String
    }

type alias Model =
    { key : Nav.Key
    , token : Maybe String
    }

type Msg =
    None
    | TryUrl Browser.UrlRequest
    | ChangeUrl Url

init : Flags -> Url -> Nav.Key -> (Model, Cmd Msg)
init flags url key =
    let
        model =
            { key = key
            , token = flags.token
            }
        cmd = case flags.token of
            Just _ -> Cmd.none
            Nothing -> Cmd.none
    in
        (model, cmd)

view : Model -> Browser.Document Msg
view m =
    { title = "Scylla"
    , body = []
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

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
