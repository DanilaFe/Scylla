module Scylla.Views exposing (..)
import Scylla.Model exposing (..)
import Scylla.Sync exposing (..)
import Scylla.Route exposing (..)
import Scylla.Fnv as Fnv
import Scylla.Messages exposing (..)
import Scylla.Login exposing (Username)
import Scylla.UserData exposing (UserData)
import Scylla.Http exposing (fullMediaUrl)
import Scylla.Api exposing (ApiUrl)
import Html.Parser
import Html.Parser.Util
import Svg
import Svg.Attributes
import Url.Builder
import Json.Decode as Decode
import Html exposing (Html, Attribute, div, input, text, button, div, span, a, h2, h3, table, td, tr, img, textarea, video, source, p)
import Html.Attributes exposing (type_, placeholder, value, href, class, style, src, id, rows, controls, src, classList)
import Html.Events exposing (onInput, onClick, preventDefaultOn)
import Html.Lazy exposing (lazy6)
import Dict exposing (Dict)
import Tuple

maybeHtml : List (Maybe (Html Msg)) -> List (Html Msg)
maybeHtml = List.filterMap (\i -> i)

contentRepositoryDownloadUrl : ApiUrl -> String -> String
contentRepositoryDownloadUrl apiUrl s =
    let
        lastIndex = Maybe.withDefault 6 <| List.head <| List.reverse <| String.indexes "/" s
        authority = String.slice 6 lastIndex s
        content = String.dropLeft (lastIndex + 1) s
    in
        (fullMediaUrl apiUrl) ++ "/download/" ++ authority ++ "/" ++ content


stringColor : String -> String
stringColor s = 
    let
        hue = String.fromFloat <| (toFloat (Fnv.hash s)) / 4294967296 * 360
    in
        "hsl(" ++ hue ++ ", 82%, 71%)"

viewFull : Model -> List (Html Msg)
viewFull model = 
    let
        room r = Maybe.map (\jr -> (r, jr))
            <| Maybe.andThen (Dict.get r)
            <| Maybe.andThen .join
            <| model.sync.rooms
        core = case model.route of
            Login -> loginView model 
            Base -> baseView model Nothing
            Room r -> baseView model <| room r
            _ -> div [] []
        errorList = errorsView model.errors
    in
        [ errorList ] ++ [ core ] 

errorsView : List String -> Html Msg
errorsView = div [ class "errors-wrapper" ] << List.indexedMap errorView

errorView : Int -> String -> Html Msg
errorView i s = div [ class "error-wrapper", onClick <| DismissError i ] [ iconView "alert-triangle", text s ]

baseView : Model -> Maybe (RoomId, JoinedRoom) -> Html Msg
baseView m jr = 
    let
        roomView = Maybe.map (\(id, r) -> joinedRoomView m id r) jr
        reconnect = reconnectView m
    in
        div [ class "base-wrapper" ] <| maybeHtml
            [ Just <| roomListView m
            , roomView
            , reconnect
            ]

reconnectView : Model -> Maybe (Html Msg)
reconnectView m = if m.connected
    then Nothing
    else Just <| div [ class "reconnect-wrapper", onClick AttemptReconnect ] [ iconView "zap", text "Disconnected. Click here to reconnect." ]

roomListView : Model -> Html Msg
roomListView m =
    let
        rooms = Maybe.withDefault (Dict.empty)
            <| Maybe.andThen .join
            <| m.sync.rooms
        groups = roomGroups
            <| Dict.toList rooms
        homeserverList = div [ class "homeservers-list" ]
            <| List.map (\(k, v) -> homeserverView m k v)
            <| Dict.toList groups
    in
        div [ class "rooms-wrapper" ]
            [ h2 [] [ text "Rooms" ]
            , input
                [ class "room-search"
                , type_ "text"
                , placeholder "Search chats..."
                , onInput UpdateSearchText 
                , value m.searchText
                ] []
            , homeserverList
            ]

roomGroups : List (String, JoinedRoom) -> Dict String (List (String, JoinedRoom))
roomGroups jrs = groupBy (homeserver << Tuple.first) jrs

homeserverView : Model -> String -> List (String, JoinedRoom) -> Html Msg
homeserverView m hs rs =
    let
        roomList = div [ class "rooms-list" ]
            <| List.map (\(rid, r) -> roomListElementView m rid r)
            <| List.sortBy (\(rid, r) -> roomDisplayName m.roomNames rid) rs
    in
        div [ class "homeserver-wrapper" ] [ h3 [] [ text hs ], roomList ]

roomListElementView : Model -> RoomId -> JoinedRoom -> Html Msg
roomListElementView m rid jr =
    let
        name = roomDisplayName m.roomNames rid
        isVisible = m.searchText == "" || (String.contains (String.toLower m.searchText) <| String.toLower name)
        isCurrentRoom = case currentRoomId m of
            Nothing -> False
            Just cr -> cr == rid
    in
        div [ classList
                [ ("room-link-wrapper", True)
                , ("active", isCurrentRoom)
                , ("hidden", not isVisible)
                ]
            ]
            <| roomNotificationCountView jr.unreadNotifications ++
                [ a [ href <| roomUrl rid ] [ text name ] ]

roomNotificationCountView : Maybe UnreadNotificationCounts -> List (Html Msg)
roomNotificationCountView ns =
    let
        wrap b = span
            [ classList
                [ ("notification-count", True)
                , ("bright", b)
                ]
            ]
        getCount f = Maybe.withDefault 0 << Maybe.andThen f
    in
        case (getCount .notificationCount ns, getCount .highlightCount ns) of
            (0, 0) -> []
            (i, 0) -> [ wrap False [ iconView "bell", text <| String.fromInt i ] ]
            (i, j) -> [ wrap True [ iconView "alert-circle", text <| String.fromInt i ] ] 

loginView : Model -> Html Msg
loginView m = div [ class "login-wrapper" ]
    [ h2 [] [ text "Log In" ]
    , input [ type_ "text", placeholder "Username", value m.loginUsername, onInput ChangeLoginUsername] []
    , input [ type_ "password", placeholder "Password", value m.loginPassword, onInput ChangeLoginPassword ] []
    , input [ type_ "text", placeholder "Homeserver URL", value m.apiUrl, onInput ChangeApiUrl ] []
    , button [ onClick AttemptLogin ] [ text "Log In" ]
    ]

joinedRoomView : Model -> RoomId -> JoinedRoom -> Html Msg
joinedRoomView m roomId jr =
    let
        typing = List.map (displayName m.userData) <| roomTypingUsers jr
        typingText = String.join ", " typing
        typingSuffix = case List.length typing of
            0 -> ""
            1 -> " is typing..."
            _ -> " are typing..."
        typingWrapper = div [ class "typing-wrapper" ] [ text <| typingText ++ typingSuffix ] 
        messageInput = div [ class "message-wrapper" ]
            [ textarea
                [ rows 1
                , onInput <| ChangeRoomText roomId
                , onEnterKey <| SendRoomText roomId
                , placeholder "Type your message here..."
                , value <| Maybe.withDefault "" <| Dict.get roomId m.roomText
                ]  []
            , button [ onClick <| SendFiles roomId ] [ iconView "file" ]
            , button [ onClick <| SendImages roomId ] [ iconView "image" ]
            , button [ onClick <| SendRoomText roomId ] [ iconView "send" ]
            ]
    in
        div [ class "room-wrapper" ]
            [ h2 [] [ text <| roomDisplayName m.roomNames roomId ]
            , lazy6 lazyMessagesView m.userData roomId jr m.apiUrl m.loginUsername m.sending
            , messageInput
            , typingWrapper
            ]

lazyMessagesView : Dict String UserData -> RoomId -> JoinedRoom -> ApiUrl -> Username -> Dict Int (RoomId, SendingMessage) -> Html Msg
lazyMessagesView ud rid jr au lu snd =
    let
        roomReceived = receivedMessagesRoom 
            <| Maybe.withDefault []
            <| Maybe.andThen .events jr.timeline
        roomSending = sendingMessagesRoom rid snd
        renderedMessages = List.map (userMessagesView ud au)
            <| mergeMessages lu
            <| roomReceived ++ roomSending
    in
        messagesWrapperView rid renderedMessages

onEnterKey : Msg -> Attribute Msg
onEnterKey msg =
    let
        eventDecoder = Decode.map2 (\l r -> (l, r)) (Decode.field "keyCode" Decode.int) (Decode.field "shiftKey" Decode.bool)
        msgFor (code, shift) = if code == 13 && not shift then Decode.succeed msg else Decode.fail "Not ENTER"
        pairTrue v = (v, True)
        decoder = Decode.map pairTrue <| Decode.andThen msgFor <| eventDecoder
    in
        preventDefaultOn "keydown" decoder

iconView : String -> Html Msg
iconView name =
    let
        url = Url.Builder.absolute [ "static", "svg", "feather-sprite.svg" ] [] 
    in
        Svg.svg
            [ Svg.Attributes.class "feather-icon"
            ] [ Svg.use [ Svg.Attributes.xlinkHref (url ++ "#" ++ name) ] [] ]

messagesWrapperView : RoomId -> List (Html Msg) -> Html Msg
messagesWrapperView rid es = div [ class "messages-wrapper", id "messages-wrapper" ]
    [ a [ class "history-link", onClick <| History rid ] [ text "Load older messages" ]
    , table [ class "messages-table" ] es
    ]

senderView : Dict String UserData -> Username -> Html Msg
senderView ud s =
    span [ style "color" <| stringColor s, class "sender-wrapper" ] [ text <| displayName ud s ]

userMessagesView : Dict String UserData -> ApiUrl -> (Username, List Message) -> Html Msg
userMessagesView ud apiUrl (u, ms) = 
    let
        wrap h = div [ class "message" ] [ h ]
    in
        tr []
            [ td [] [ senderView ud u ]
            , td [] <| List.map wrap <| List.filterMap (messageView ud apiUrl) ms
            ]

messageView : Dict String UserData -> ApiUrl -> Message -> Maybe (Html Msg)
messageView ud apiUrl msg = case msg of
    Sending t -> Just <| sendingMessageView t
    Received re -> roomEventView ud apiUrl re

sendingMessageView : SendingMessage -> Html Msg
sendingMessageView msg = case msg.body of
    TextMessage t -> span [ class "sending"] [ text t ]

roomEventView : Dict String UserData -> ApiUrl -> RoomEvent -> Maybe (Html Msg)
roomEventView ud apiUrl re =
    let
        msgtype = Decode.decodeValue (Decode.field "msgtype" Decode.string) re.content
    in
        case msgtype of
            Ok "m.text" -> roomEventTextView re
            Ok "m.notice" -> roomEventNoticeView re
            Ok "m.emote" -> roomEventEmoteView ud re
            Ok "m.image" -> roomEventImageView apiUrl re
            Ok "m.file" -> roomEventFileView apiUrl re
            Ok "m.video" -> roomEventVideoView apiUrl re
            _ -> Nothing

roomEventFormattedContent : RoomEvent -> Maybe (List (Html Msg))
roomEventFormattedContent re = Maybe.map Html.Parser.Util.toVirtualDom
    <| Maybe.andThen (Result.toMaybe << Html.Parser.run )
    <| Result.toMaybe
    <| Decode.decodeValue (Decode.field "formatted_body" Decode.string) re.content

roomEventContent : (List (Html Msg) -> Html Msg) -> RoomEvent -> Maybe (Html Msg)
roomEventContent f re =
    let
        body = Decode.decodeValue (Decode.field "body" Decode.string) re.content
        customHtml = roomEventFormattedContent re
    in
        case customHtml of
            Just c -> Just <| f c
            Nothing -> Maybe.map (f << List.singleton << text) <| Result.toMaybe body

roomEventEmoteView : Dict String UserData -> RoomEvent -> Maybe (Html Msg)
roomEventEmoteView ud re =
    let
        emoteText = "* " ++ displayName ud re.sender ++ " "
    in
        roomEventContent (\cs -> span [] (text emoteText :: cs)) re

roomEventNoticeView : RoomEvent -> Maybe (Html Msg)
roomEventNoticeView = roomEventContent (span [ class "message-notice" ])

roomEventTextView : RoomEvent -> Maybe (Html Msg)
roomEventTextView = roomEventContent (span [])

roomEventImageView : ApiUrl -> RoomEvent -> Maybe (Html Msg)
roomEventImageView apiUrl re =
    let
        body = Decode.decodeValue (Decode.field "url" Decode.string) re.content
    in
        Maybe.map (\s -> img [ class "message-image", src s ] [])
            <| Maybe.map (contentRepositoryDownloadUrl apiUrl)
            <| Result.toMaybe body

roomEventFileView : ApiUrl -> RoomEvent -> Maybe (Html Msg)
roomEventFileView apiUrl re =
    let
        decoder = Decode.map2 (\l r -> (l, r)) (Decode.field "url" Decode.string) (Decode.field "body" Decode.string)
        fileData = Decode.decodeValue decoder re.content
    in
        Maybe.map (\(url, name) -> a [ href url, class "file-wrapper" ] [ iconView "file", text name ])
        <| Maybe.map (\(url, name) -> (contentRepositoryDownloadUrl apiUrl url, name))
        <| Result.toMaybe fileData

roomEventVideoView : ApiUrl -> RoomEvent -> Maybe (Html Msg)
roomEventVideoView apiUrl re =
    let
        decoder = Decode.map2 (\l r -> (l, r))
            (Decode.field "url" Decode.string)
            (Decode.field "info" <| Decode.field "mimetype" Decode.string)
        videoData = Decode.decodeValue decoder re.content
    in
        Maybe.map (\(url, t) -> video [ controls True ] [ source [ src url, type_ t ] [] ])
        <| Maybe.map (\(url, type_) -> (contentRepositoryDownloadUrl apiUrl url, type_))
        <| Result.toMaybe videoData
