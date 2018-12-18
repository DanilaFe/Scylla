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
import Url exposing (Url)
import Url.Parser exposing (parse)
import Url.Builder
import Json.Encode
import Json.Decode
import Time exposing (every)
import Html exposing (div, text)
import Http
import Dict
import Task

syncTimeout = 10000
typingTimeout = 2000

type alias Flags =
    { token : Maybe String
    }

init : Flags -> Url -> Nav.Key -> (Model, Cmd Msg)
init flags url key =
    let
        model =
            { key = key
            , route = Maybe.withDefault Unknown <| parse Scylla.Route.route url
            , token = flags.token
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
        cmd = case flags.token of
            Just _ -> Cmd.none
            Nothing -> getStoreValuePort "scylla.loginInfo"
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
    Err e -> (m, userData m.apiUrl (Maybe.withDefault "" m.token) s)

updateSendRoomText : Model -> RoomId -> (Model, Cmd Msg)
updateSendRoomText m r =
    let
        token = Maybe.withDefault "" m.token
        message = Maybe.andThen (\s -> if s == "" then Nothing else Just s)
            <| Dict.get r m.roomText
        combinedCmd = case message of
            Nothing -> Cmd.none
            Just s -> Cmd.batch
                [ sendTextMessage m.apiUrl token m.transactionId r s
                , sendTypingIndicator m.apiUrl token r m.loginUsername False typingTimeout
                , setStoreValuePort ("scylla.loginInfo", Json.Encode.string
                    <| encodeLoginInfo
                    <| LoginInfo (Maybe.withDefault "" m.token) m.apiUrl m.loginUsername (m.transactionId + 1))
                ]
    in
        ({ m | roomText = Dict.insert r "" m.roomText, transactionId = m.transactionId + 1 }, combinedCmd)

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
    Err e  -> (model, Cmd.none)

updateSyncResponse : Model -> Result Http.Error SyncResponse -> Bool -> (Model, Cmd Msg)
updateSyncResponse model r notify =
    let
        token = Maybe.withDefault "" model.token
        nextBatch = Result.withDefault model.sync.nextBatch
            <| Result.map .nextBatch r
        syncCmd = sync nextBatch model.apiUrl token
        newUsers sr = List.filter (\s -> not <| Dict.member s model.userData) <| roomsUsers sr
        newUserCmd sr = Cmd.batch
            <| List.map (userData model.apiUrl
            <| Maybe.withDefault "" model.token)
            <| newUsers sr
        notification sr = findFirstBy
            (\(s, e) -> e.originServerTs)
            (\(s, e) -> e.sender /= model.loginUsername)
            <| notificationEvents sr
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
