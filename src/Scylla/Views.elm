module Scylla.Views exposing (..)
import Scylla.Model exposing (..)
import Scylla.Sync exposing (..)
import Scylla.Route exposing (..)
import Scylla.Fnv as Fnv
import Scylla.Login exposing (Username)
import Scylla.Http exposing (fullMediaUrl)
import Scylla.Api exposing (ApiUrl)
import Html.Parser
import Html.Parser.Util
import Svg
import Svg.Attributes
import Url.Builder
import Json.Decode as Decode
import Html exposing (Html, Attribute, div, input, text, button, div, span, a, h2, table, td, tr, img, textarea)
import Html.Attributes exposing (type_, value, href, class, style, src, id, rows)
import Html.Events exposing (onInput, onClick, preventDefaultOn)
import Dict

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
            <| Maybe.andThen .join model.sync.rooms
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

baseView : Model -> Maybe (String, JoinedRoom) -> Html Msg
baseView m jr = 
    let
        roomView = case jr of
            Just (id, r) -> joinedRoomView m id r
            Nothing -> div [] []
    in
        div [ class "base-wrapper" ]
        [ roomListView m
        , roomView
        ]

roomListView : Model -> Html Msg
roomListView m =
    let
        rooms = Maybe.withDefault (Dict.empty) <| Maybe.andThen .join <| m.sync.rooms
        roomList = div [ class "rooms-list" ] <| Dict.values <| Dict.map roomListElementView rooms
    in
        div [ class "rooms-wrapper" ]
            [ h2 [] [ text "Rooms" ]
            , roomList
            ]

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

joinedRoomView : Model -> RoomId -> JoinedRoom -> Html Msg
joinedRoomView m roomId jr =
    let
        events = Maybe.withDefault [] <| Maybe.andThen .events jr.timeline
        renderedEvents = List.filterMap (eventView m) events
        eventWrapper = eventWrapperView m roomId renderedEvents
        typing = List.map (displayName m) <| roomTypingUsers jr
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
            [ h2 [] [ text <| Maybe.withDefault "<No Name>" <| roomName jr ]
            , eventWrapper
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

eventWrapperView : Model -> RoomId -> List (Html Msg) -> Html Msg
eventWrapperView m rid es = div [ class "events-wrapper", id "events-wrapper" ]
    [ a [ class "history-link", onClick <| History rid ] [ text "Load older messages" ]
    , table [ class "events-table" ] es
    ]

eventView : Model -> RoomEvent -> Maybe (Html Msg)
eventView m re = 
    let
        viewFunction = case re.type_ of
            "m.room.message" -> Just messageView
            _ -> Nothing
        createRow mhtml = tr []
            [ td [] [ eventSenderView m re.sender ]
            , td [] [ mhtml ]
            ]
    in
        Maybe.map createRow
            <| Maybe.andThen (\f -> f m re) viewFunction

eventSenderView : Model -> Username -> Html Msg
eventSenderView m s =
    span [ style "background-color" <| stringColor s, class "sender-wrapper" ] [ text <| displayName m s ]

messageView : Model -> RoomEvent -> Maybe (Html Msg)
messageView m re =
    let
        msgtype = Decode.decodeValue (Decode.field "msgtype" Decode.string) re.content
    in
        case msgtype of
            Ok "m.text" -> messageTextView m re
            Ok "m.image" -> messageImageView m re
            _ -> Nothing

messageTextView : Model -> RoomEvent -> Maybe (Html Msg)
messageTextView m re =
    let
        body = Decode.decodeValue (Decode.field "body" Decode.string) re.content
        customHtml = Maybe.map Html.Parser.Util.toVirtualDom
            <| Maybe.andThen (Result.toMaybe << Html.Parser.run )
            <| Result.toMaybe
            <| Decode.decodeValue (Decode.field "formatted_body" Decode.string) re.content
        wrap mtext = span [] [ text mtext ]
    in
        case customHtml of
            Just c -> Just <| div [ class "markdown-wrapper" ] c
            Nothing -> Maybe.map wrap <| Result.toMaybe body

messageImageView : Model -> RoomEvent -> Maybe (Html Msg)
messageImageView m re =
    let
        body = Decode.decodeValue (Decode.field "url" Decode.string) re.content
    in
        Maybe.map (\s -> img [ class "message-image", src s ] [])
            <| Maybe.map (contentRepositoryDownloadUrl m.apiUrl)
            <| Result.toMaybe body
