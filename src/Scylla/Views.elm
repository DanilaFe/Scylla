module Scylla.Views exposing (..)
import Scylla.Model exposing (..)
import Scylla.Sync exposing (..)
import Scylla.Route exposing (..)
import Scylla.Fnv as Fnv
import Scylla.Room exposing (..)
import Scylla.Messages exposing (..)
import Scylla.Login exposing (Username)
import Scylla.Http exposing (fullMediaUrl)
import Scylla.Api exposing (ApiUrl)
import Html.Parser
import Html.Parser.Util
import Svg
import Svg.Attributes
import Url.Builder
import Json.Decode as Decode
import Html exposing (Html, Attribute, div, input, text, button, div, span, a, h2, h3, table, td, tr, img, textarea, video, source, p)
import Html.Attributes exposing (type_, value, href, class, style, src, id, rows, controls, src)
import Html.Events exposing (onInput, onClick, preventDefaultOn)
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
        room r = Maybe.map (\rd -> (r, rd))
            <| roomData model r
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

baseView : Model -> Maybe (String, RoomData) -> Html Msg
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
            <| List.map (\(k, v) -> homeserverView k v)
            <| Dict.toList groups
    in
        div [ class "rooms-wrapper" ]
            [ h2 [] [ text "Rooms" ]
            , homeserverList
            ]

roomGroups : List (String, JoinedRoom) -> Dict String (List (String, JoinedRoom))
roomGroups jrs = groupBy (homeserver << Tuple.first) jrs

homeserverView : String -> List (String, JoinedRoom) -> Html Msg
homeserverView hs rs =
    let
        roomList = div [ class "rooms-list" ] <| List.map (\(rid, r) -> roomListElementView rid r) rs
    in
        div [ class "homeserver-wrapper" ] [ h3 [] [ text hs ], roomList ]

roomListElementView : String -> JoinedRoom -> Html Msg
roomListElementView s jr =
    let
        name = Maybe.withDefault "<No Name>"  <| roomName jr
    in
        div [ class "room-link-wrapper" ]
            [ a [ href <| roomUrl s ] [ text name ]
            , roomNotificationCountView jr.unreadNotifications
            ]

roomNotificationCountView : Maybe UnreadNotificationCounts -> Html Msg
roomNotificationCountView ns =
    let
        spanNumber = case Maybe.andThen .notificationCount ns of
            Nothing -> ""
            Just 0 -> ""
            Just i -> String.fromInt i
        spanSuffix = case Maybe.andThen .highlightCount ns of
            Nothing -> ""
            Just 0 -> ""
            Just i -> "!"
    in
        span [ class "notification-count" ] [ text (spanNumber ++ spanSuffix) ]

loginView : Model -> Html Msg
loginView m = div [ class "login-wrapper" ]
    [ h2 [] [ text "Log In" ]
    , input [ type_ "text", value m.loginUsername, onInput ChangeLoginUsername] []
    , input [ type_ "password", value m.loginPassword, onInput ChangeLoginPassword ] []
    , input [ type_ "text", value m.apiUrl, onInput ChangeApiUrl ] []
    , button [ onClick AttemptLogin ] [ text "Log In" ]
    ]

joinedRoomView : Model -> RoomId -> RoomData -> Html Msg
joinedRoomView m roomId rd =
    let
        renderedMessages = List.map (userMessagesView m) <| mergeMessages m.loginUsername <| extractMessages rd
        messagesWrapper = messagesWrapperView m roomId renderedMessages
        typing = List.map (displayName m) <| roomTypingUsers rd.joinedRoom
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
                , value <| Maybe.withDefault "" <| Dict.get roomId m.roomText
                ]  []
            , button [ onClick <| SendFiles roomId ] [ iconView "file" ]
            , button [ onClick <| SendImages roomId ] [ iconView "image" ]
            , button [ onClick <| SendRoomText roomId ] [ iconView "send" ]
            ]
    in
        div [ class "room-wrapper" ]
            [ h2 [] [ text <| Maybe.withDefault "<No Name>" <| roomName rd.joinedRoom ]
            , messagesWrapper
            , typingWrapper
            , messageInput
            ]

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

messagesWrapperView : Model -> RoomId -> List (Html Msg) -> Html Msg
messagesWrapperView m rid es = div [ class "messages-wrapper", id "messages-wrapper" ]
    [ a [ class "history-link", onClick <| History rid ] [ text "Load older messages" ]
    , table [ class "messages-table" ] es
    ]

senderView : Model -> Username -> Html Msg
senderView m s =
    span [ style "color" <| stringColor s, class "sender-wrapper" ] [ text <| displayName m s ]

userMessagesView : Model -> (Username, List Message) -> Html Msg
userMessagesView m (u, ms) = 
    let
        wrap h = div [ class "message" ] [ h ]
    in
        tr []
            [ td [] [ senderView m u ]
            , td [] <| List.map wrap <| List.filterMap (messageView m) ms
            ]

messageView : Model -> Message -> Maybe (Html Msg)
messageView m msg = case msg of
    Sending t -> Just <| sendingMessageView m t
    Received re -> roomEventView m re

sendingMessageView : Model -> SendingMessage -> Html Msg
sendingMessageView m msg = case msg.body of
    TextMessage t -> span [ class "sending"] [ text t ]

roomEventView : Model -> RoomEvent -> Maybe (Html Msg)
roomEventView m re =
    let
        msgtype = Decode.decodeValue (Decode.field "msgtype" Decode.string) re.content
    in
        case msgtype of
            Ok "m.text" -> roomEventTextView m re
            Ok "m.image" -> roomEventImageView m re
            Ok "m.file" -> roomEventFileView m re
            Ok "m.video" -> roomEventVideoView m re
            _ -> Nothing

roomEventTextView : Model -> RoomEvent -> Maybe (Html Msg)
roomEventTextView m re =
    let
        body = Decode.decodeValue (Decode.field "body" Decode.string) re.content
        customHtml = Maybe.map Html.Parser.Util.toVirtualDom
            <| Maybe.andThen (Result.toMaybe << Html.Parser.run )
            <| Result.toMaybe
            <| Decode.decodeValue (Decode.field "formatted_body" Decode.string) re.content
    in
        case customHtml of
            Just c -> Just <| div [] c
            Nothing -> Maybe.map (p [] << List.singleton << text) <| Result.toMaybe body

roomEventImageView : Model -> RoomEvent -> Maybe (Html Msg)
roomEventImageView m re =
    let
        body = Decode.decodeValue (Decode.field "url" Decode.string) re.content
    in
        Maybe.map (\s -> img [ class "message-image", src s ] [])
            <| Maybe.map (contentRepositoryDownloadUrl m.apiUrl)
            <| Result.toMaybe body

roomEventFileView : Model -> RoomEvent -> Maybe (Html Msg)
roomEventFileView m re =
    let
        decoder = Decode.map2 (\l r -> (l, r)) (Decode.field "url" Decode.string) (Decode.field "body" Decode.string)
        fileData = Decode.decodeValue decoder re.content
    in
        Maybe.map (\(url, name) -> a [ href url, class "file-wrapper" ] [ iconView "file", text name ])
        <| Maybe.map (\(url, name) -> (contentRepositoryDownloadUrl m.apiUrl url, name))
        <| Result.toMaybe fileData

roomEventVideoView : Model -> RoomEvent -> Maybe (Html Msg)
roomEventVideoView m re =
    let
        decoder = Decode.map2 (\l r -> (l, r))
            (Decode.field "url" Decode.string)
            (Decode.field "info" <| Decode.field "mimetype" Decode.string)
        videoData = Decode.decodeValue decoder re.content
    in
        Maybe.map (\(url, t) -> video [ controls True ] [ source [ src url, type_ t ] [] ])
        <| Maybe.map (\(url, type_) -> (contentRepositoryDownloadUrl m.apiUrl url, type_))
        <| Result.toMaybe videoData
