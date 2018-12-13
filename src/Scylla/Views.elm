module Scylla.Views exposing (..)
import Scylla.Model exposing (..)
import Scylla.Sync exposing (..)
import Scylla.Route exposing (..)
import Scylla.Fnv as Fnv
import Scylla.Login exposing (Username)
import Svg
import Svg.Attributes
import Url.Builder
import Json.Decode as Decode
import Html exposing (Html, div, input, text, button, div, span, a, h2, table, td, tr)
import Html.Attributes exposing (type_, value, href, class, style)
import Html.Events exposing (onInput, onClick)
import Dict

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
errorsView = div [] << List.map errorView

errorView : String -> Html Msg
errorView s = div [] [ text s ]

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
        a [ href <| roomUrl s ] [ text name ]

loginView : Model -> Html Msg
loginView m = div [ class "login-wrapper" ]
    [ h2 [] [ text "Log In" ]
    , input [ type_ "text", value m.loginUsername, onInput ChangeLoginUsername] []
    , input [ type_ "password", value m.loginPassword, onInput ChangeLoginPassword ] []
    , input [ type_ "text", value m.apiUrl, onInput ChangeApiUrl ] []
    , button [ onClick AttemptLogin ] [ text "Log In" ]
    ]

joinedRoomView : Model -> String -> JoinedRoom -> Html Msg
joinedRoomView m roomId jr =
    let
        events = Maybe.withDefault [] <| Maybe.andThen .events jr.timeline
        renderedEvents = List.filterMap (eventView m) events
        eventWrapper = eventWrapperView m renderedEvents
        messageInput = div [ class "message-wrapper" ]
            [ input
                [ type_ "text"
                , onInput <| ChangeRoomText roomId
                , value <| Maybe.withDefault "" <| Dict.get roomId m.roomText
                ]  []
            , button [ onClick <| SendRoomText roomId ] [ iconView "send" ]
            ]
    in
        div [ class "room-wrapper" ]
            [ h2 [] [ text <| Maybe.withDefault "<No Name>" <| roomName jr ]
            , eventWrapper
            , messageInput
            ]

iconView : String -> Html Msg
iconView name =
    let
        url = Url.Builder.absolute [ "static", "svg", "feather-sprite.svg" ] [] 
    in
        Svg.svg
            [ Svg.Attributes.class "feather-icon"
            ] [ Svg.use [ Svg.Attributes.xlinkHref (url ++ "#" ++ name) ] [] ]

eventWrapperView : Model -> List (Html Msg) -> Html Msg
eventWrapperView m es = div [ class "events-wrapper" ] [ table [ class "events-table" ] es ]

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
            _ -> Nothing

messageTextView : Model -> RoomEvent -> Maybe (Html Msg)
messageTextView m re =
    let
        body = Decode.decodeValue (Decode.field "body" Decode.string) re.content
        wrap mtext = span [] [ text mtext ]
    in
        Maybe.map wrap <| Result.toMaybe body
