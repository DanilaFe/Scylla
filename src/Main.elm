import Browser exposing (application, UrlRequest(..))
import Browser.Navigation as Nav
import Browser.Dom exposing (Viewport, setViewportOf)
import Scylla.Sync exposing (..)
import Scylla.Login exposing (..)
import Scylla.Api exposing (..)
import Scylla.Model exposing (..)
import Scylla.Http exposing (..)
import Scylla.Views exposing (viewFull)
import Scylla.Route exposing (Route(..), RoomId)
import Scylla.UserData exposing (..)
import Scylla.Notification exposing (..)
import Scylla.Storage exposing (..)
import Scylla.Markdown exposing (..)
import Scylla.AccountData exposing (..)
import Url exposing (Url)
import Url.Parser exposing (parse)
import Url.Builder
import Json.Encode
import Json.Decode
import Time exposing (every)
import Html exposing (div, text)
import File exposing (File)
import File.Select as Select
import Http
import Dict
import Task

syncTimeout = 10000
typingTimeout = 2000

init : () -> Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
    let
        model =
            { key = key
            , route = Maybe.withDefault Unknown <| parse Scylla.Route.route url
            , token = Nothing
            , loginUsername = ""
            , loginPassword = ""
            , apiUrl = "https://matrix.org"
            , sync =
                { nextBatch = ""
                , rooms = Nothing
                , presence = Nothing
                , accountData = Nothing
                }
            , errors = []
            , roomText = Dict.empty
            , transactionId = 0
            , userData = Dict.empty
            }
        cmd = getStoreValuePort "scylla.loginInfo"
    in
        (model, cmd)

view : Model -> Browser.Document Msg
view m =
    { title = "Scylla"
    , body = viewFull m
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    ChangeApiUrl u -> ({ model | apiUrl = u }, Cmd.none)
    ChangeLoginUsername u -> ({ model | loginUsername = u }, Cmd.none)
    ChangeLoginPassword p -> ({ model | loginPassword = p }, Cmd.none)
    AttemptLogin -> (model, Scylla.Http.login model.apiUrl model.loginUsername model.loginPassword) -- TODO 
    TryUrl urlRequest -> updateTryUrl model urlRequest
    OpenRoom s -> (model, Nav.pushUrl model.key <| roomUrl s)
    ChangeRoute r -> updateChangeRoute model r
    ViewportAfterMessage v -> updateViewportAfterMessage model v
    ViewportChangeComplete _ -> (model, Cmd.none)
    ReceiveLoginResponse a r -> updateLoginResponse model a r
    ReceiveFirstSyncResponse r -> updateSyncResponse model r False
    ReceiveSyncResponse r -> updateSyncResponse model r True
    ReceiveUserData s r -> updateUserData model s r
    ChangeRoomText r t -> updateChangeRoomText model r t
    SendRoomText r -> updateSendRoomText model r
    SendRoomTextResponse r -> (model, Cmd.none)
    ReceiveCompletedReadMarker r -> (model, Cmd.none)
    ReceiveCompletedTypingIndicator r -> (model, Cmd.none)
    ReceiveStoreData d -> updateStoreData model d
    TypingTick _ -> updateTypingTick model
    History r -> updateHistory model r
    ReceiveHistoryResponse r hr -> updateHistoryResponse model r hr
    SendImages rid -> (model, Select.files [ "image/png" ] <| ImagesSelected rid)
    SendFiles rid -> (model, Select.files [ "application/*" ] <| FilesSelected rid)
    ImagesSelected rid f fs -> updateUploadSelected model rid f fs (ImageUploadComplete rid)
    FilesSelected rid f fs -> updateUploadSelected model rid f fs (FileUploadComplete rid)
    ImageUploadComplete rid mime ur -> updateImageUploadComplete model rid mime ur
    FileUploadComplete rid mime ur -> updateFileUploadComplete model rid mime ur
    SendImageResponse _ -> (model, Cmd.none)
    SendFileResponse _ -> (model, Cmd.none)
    ReceiveMarkdown md -> updateMarkdown model md
    DismissError i -> updateDismissError model i

updateDismissError : Model -> Int -> (Model, Cmd Msg)
updateDismissError m i = ({ m | errors = (List.take i m.errors) ++ (List.drop (i+1) m.errors)}, Cmd.none)

updateMarkdown : Model -> MarkdownResponse -> (Model, Cmd Msg)
updateMarkdown m { roomId, text, markdown } =
    let
        storeValueCmd = setStoreValuePort ("scylla.loginInfo", Json.Encode.string
            <| encodeLoginInfo
            <| LoginInfo (Maybe.withDefault "" m.token) m.apiUrl m.loginUsername (m.transactionId + 1))
        sendMessageCmd = sendMarkdownMessage m.apiUrl (Maybe.withDefault "" m.token) (m.transactionId + 1) roomId text markdown
    in
        ({ m | transactionId = m.transactionId + 1 }, Cmd.batch [ storeValueCmd, sendMessageCmd ])

updateFileUploadComplete : Model -> RoomId -> String -> (Result Http.Error String) -> (Model, Cmd Msg)
updateFileUploadComplete m rid mime ur =
    let
        command = case ur of
            Ok u -> sendFileMessage m.apiUrl (Maybe.withDefault "" m.token) m.transactionId rid mime u
            _ -> Cmd.none
        newErrors = case ur of
            Err e -> [ "Error uploading file. Please check your internet connection and try again." ]
            _ -> []
    in
        ({ m | errors = newErrors ++ m.errors, transactionId = m.transactionId + 1}, command)

updateImageUploadComplete : Model -> RoomId -> String -> (Result Http.Error String) -> (Model, Cmd Msg)
updateImageUploadComplete m rid mime ur =
    let
        command = case ur of
            Ok u -> sendImageMessage m.apiUrl (Maybe.withDefault "" m.token) m.transactionId rid mime u
            _ -> Cmd.none
        newErrors = case ur of
            Err e -> [ "Error uploading image. Please check your internet connection and try again." ]
            _ -> []
    in
        ({ m | transactionId = m.transactionId + 1}, command)

updateUploadSelected : Model -> RoomId -> File -> List File -> (String -> Result Http.Error String -> Msg) -> (Model, Cmd Msg)
updateUploadSelected m rid f fs msg =
    let
        uploadCmds = List.map (uploadMediaFile m.apiUrl (Maybe.withDefault "" m.token) msg) (f::fs)
    in
        (m, Cmd.batch uploadCmds)

updateHistoryResponse : Model -> RoomId -> Result Http.Error HistoryResponse -> (Model, Cmd Msg)
updateHistoryResponse m r hr =
    let
        newUsersCmd h = Cmd.batch
            <| List.map (userData m.apiUrl (Maybe.withDefault "" m.token))
            <| newUsers m
            <| uniqueBy (\s -> s)
            <| List.map .sender
            <| h.chunk
    in
        case hr of
            Ok h -> ({ m | sync = appendHistoryResponse m.sync r h }, newUsersCmd h)
            Err _ -> ({ m | errors = "Unable to load older history from server"::m.errors }, Cmd.none)

updateHistory : Model -> RoomId -> (Model, Cmd Msg)
updateHistory m r =
    let
        prevBatch = Maybe.andThen .prevBatch
            <| Maybe.andThen .timeline 
            <| Maybe.andThen (Dict.get r)  
            <| Maybe.andThen .join 
            <| m.sync.rooms
        command = case prevBatch of
            Just pv -> getHistory m.apiUrl (Maybe.withDefault "" m.token) r pv
            Nothing -> Cmd.none
    in
        (m, command)

updateChangeRoomText : Model -> RoomId -> String -> (Model, Cmd Msg)
updateChangeRoomText m roomId text = 
    let
        typingIndicator = case (text, Dict.get roomId m.roomText) of
            ("", _) -> Just False
            (_, Just "") -> Just True
            (_, Nothing) -> Just True
            _ -> Nothing
        command = case typingIndicator of
            Just b -> sendTypingIndicator m.apiUrl (Maybe.withDefault "" m.token) roomId m.loginUsername b typingTimeout
            _ -> Cmd.none
    in
        ({ m | roomText = Dict.insert roomId text m.roomText}, command)

updateTypingTick : Model -> (Model, Cmd Msg)
updateTypingTick m = 
    let
        command = case currentRoomId m of
            Just rid -> sendTypingIndicator m.apiUrl (Maybe.withDefault "" m.token) rid m.loginUsername True typingTimeout
            Nothing -> Cmd.none
    in
        (m, command)


updateStoreData : Model -> Json.Encode.Value -> (Model, Cmd Msg)
updateStoreData m d = case (Json.Decode.decodeValue storeDataDecoder d) of
    Ok { key, value } -> case key of
        "scylla.loginInfo" -> updateLoginInfo m value
        _ -> (m, Cmd.none)
    Err _ -> (m, Cmd.none)

updateLoginInfo : Model -> Json.Encode.Value -> (Model, Cmd Msg)
updateLoginInfo m s = case Json.Decode.decodeValue (Json.Decode.map decodeLoginInfo Json.Decode.string) s of
    Ok (Just { token, apiUrl, username, transactionId }) ->
        (
            { m | token = Just token
            , apiUrl = apiUrl
            , loginUsername = username
            , transactionId = transactionId
            }
        , firstSync apiUrl token
        )
    _ -> (m, Nav.pushUrl m.key <| Url.Builder.absolute [ "login" ] [])

updateChangeRoute : Model -> Route -> (Model, Cmd Msg)
updateChangeRoute m r =
    let
        joinedRoom = case r of
            Room rid -> Maybe.andThen (Dict.get rid) <| Maybe.andThen .join <| m.sync.rooms
            _ -> Nothing
        lastMessage = Maybe.andThen (findLastEvent (((==) "m.room.message") << .type_)) <| Maybe.andThen .events <| Maybe.andThen .timeline joinedRoom
        readMarkerCmd = case (r, lastMessage) of
            (Room rid, Just re) -> setReadMarkers m.apiUrl (Maybe.withDefault "" m.token) rid re.eventId <| Just re.eventId
            _ -> Cmd.none
    in
        ({ m | route = r }, readMarkerCmd)

updateViewportAfterMessage : Model -> Result Browser.Dom.Error Viewport -> (Model, Cmd Msg)
updateViewportAfterMessage m vr = 
    let
        cmd vp = if vp.scene.height - (vp.viewport.y + vp.viewport.height ) < 100
            then Task.attempt ViewportChangeComplete <| setViewportOf "events-wrapper" vp.viewport.x vp.scene.height
            else Cmd.none
    in
        (m, Result.withDefault Cmd.none <| Result.map cmd vr)

updateUserData : Model -> String -> Result Http.Error UserData -> (Model, Cmd Msg)
updateUserData m s r = case r of
    Ok ud -> ({ m | userData = Dict.insert s ud m.userData }, Cmd.none)
    Err e -> ({ m | errors = ("Failed to retrieve user data for user " ++ s)::m.errors }, userData m.apiUrl (Maybe.withDefault "" m.token) s)

updateSendRoomText : Model -> RoomId -> (Model, Cmd Msg)
updateSendRoomText m r =
    let
        token = Maybe.withDefault "" m.token
        message = Maybe.andThen (\s -> if s == "" then Nothing else Just s)
            <| Dict.get r m.roomText
        combinedCmd = case message of
            Nothing -> Cmd.none
            Just s -> Cmd.batch
                [ requestMarkdownPort { roomId = r, text = s }
                , sendTypingIndicator m.apiUrl token r m.loginUsername False typingTimeout
                ]
    in
        ({ m | roomText = Dict.insert r "" m.roomText }, combinedCmd)

updateTryUrl : Model -> Browser.UrlRequest -> (Model, Cmd Msg)
updateTryUrl m ur = case ur of
    Internal u -> (m, Nav.pushUrl m.key (Url.toString u))
    _ -> (m, Cmd.none)

updateLoginResponse : Model -> ApiUrl -> Result Http.Error LoginResponse -> (Model, Cmd Msg)
updateLoginResponse model a r = case r of
    Ok lr -> ( { model | token = Just lr.accessToken, loginUsername = lr.userId, apiUrl = a }, Cmd.batch
        [ firstSync model.apiUrl lr.accessToken
        , Nav.pushUrl model.key <| Url.Builder.absolute [] []
        , setStoreValuePort ("scylla.loginInfo", Json.Encode.string
            <| encodeLoginInfo 
            <| LoginInfo lr.accessToken model.apiUrl lr.userId model.transactionId)
        ] )
    Err e  -> ({ model | errors = "Failed to log in. Are your username and password correct?"::model.errors }, Cmd.none)

updateSyncResponse : Model -> Result Http.Error SyncResponse -> Bool -> (Model, Cmd Msg)
updateSyncResponse model r notify =
    let
        token = Maybe.withDefault "" model.token
        nextBatch = Result.withDefault model.sync.nextBatch
            <| Result.map .nextBatch r
        syncCmd = sync model.apiUrl token nextBatch
        newUserCmd sr = Cmd.batch
            <| List.map (userData model.apiUrl
            <| Maybe.withDefault "" model.token)
            <| newUsers model
            <| allUsers sr
        notification sr = findFirstBy
            (\(s, e) -> e.originServerTs)
            (\(s, e) -> e.sender /= model.loginUsername)
            <| joinedRoomNotificationEvents sr
        notificationCmd sr = if notify
            then Maybe.withDefault Cmd.none
                    <| Maybe.map (\(s, e) -> sendNotificationPort
                    { name = displayName model e.sender
                    , text = notificationText e
                    , room = s
                    }) <| notification sr
            else Cmd.none
        room = currentRoomId model
        roomMessages sr = case room of
            Just rid -> List.filter (((==) "m.room.message") << .type_)
                <| Maybe.withDefault []
                <| Maybe.andThen .events
                <| Maybe.andThen .timeline
                <| Maybe.andThen (Dict.get rid)
                <| Maybe.andThen .join
                <| sr.rooms
            Nothing -> []
        setScrollCmd sr = if List.isEmpty 
            <| roomMessages sr
                then Cmd.none
                else Task.attempt ViewportAfterMessage (Browser.Dom.getViewportOf "events-wrapper")
        setReadReceiptCmd sr = case (room, List.head <| List.reverse <| roomMessages sr) of
            (Just rid, Just re) -> setReadMarkers model.apiUrl token rid re.eventId <| Just re.eventId
            _ -> Cmd.none
    in
        case r of
            Ok sr -> ({ model | sync = mergeSyncResponse model.sync sr }, Cmd.batch
                [ syncCmd
                , newUserCmd sr
                , notificationCmd sr
                , setScrollCmd sr
                , setReadReceiptCmd sr
                ])
            _ -> (model, syncCmd)

subscriptions : Model -> Sub Msg
subscriptions m =
    let
        currentText = Maybe.withDefault ""
            <| Maybe.andThen (\rid -> Dict.get rid m.roomText)
            <| currentRoomId m
        typingTimer = case currentText of
            "" -> Sub.none
            _ -> every typingTimeout TypingTick
    in
        Sub.batch
            [ onNotificationClickPort OpenRoom
            , receiveStoreValuePort ReceiveStoreData
            , typingTimer
            , receiveMarkdownPort ReceiveMarkdown
            ]

onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest = TryUrl

onUrlChange : Url -> Msg
onUrlChange = ChangeRoute << Maybe.withDefault Unknown << parse Scylla.Route.route

main = application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = onUrlRequest
    , onUrlChange = onUrlChange
    }
