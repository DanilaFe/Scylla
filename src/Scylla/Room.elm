module Scylla.Room exposing (..)
import Scylla.Route exposing (RoomId)
import Scylla.Sync exposing (SyncResponse)
import Scylla.Login exposing (Username)
import Scylla.Sync exposing (HistoryResponse)
import Scylla.Sync.Events exposing (MessageEvent, StateEvent, toStateEvent, toMessageEvent)
import Scylla.Sync.AccountData exposing (AccountData, getDirectMessages, applyAccountData)
import Scylla.Sync.Rooms exposing (JoinedRoom, UnreadNotificationCounts, Ephemeral)
import Scylla.ListUtils exposing (findFirst, uniqueBy)
import Json.Decode as Decode exposing (Decoder, Value, decodeValue, field, string, list)
import Dict exposing (Dict)

type alias RoomState = Dict (String, String) Value

type alias RoomData =
    { roomState : RoomState
    , messages : List (MessageEvent)
    , accountData : AccountData
    , ephemeral : Ephemeral
    , unreadNotifications : UnreadNotificationCounts
    , prevHistoryBatch : Maybe String
    , text : String
    }

type alias OpenRooms = Dict RoomId RoomData

emptyOpenRooms : OpenRooms
emptyOpenRooms = Dict.empty

emptyRoomData : RoomData
emptyRoomData =
    { roomState = Dict.empty
    , messages = []
    , accountData = { events = Just [] }
    , ephemeral = { events = Just [] }
    , unreadNotifications =
        { highlightCount = Just 0
        , notificationCount = Just 0
        }
    , prevHistoryBatch = Nothing
    , text = ""
    }

changeRoomStateEvent : StateEvent -> RoomState -> RoomState
changeRoomStateEvent se = Dict.insert (se.type_, se.stateKey) se.content

changeRoomStateEvents : List StateEvent -> RoomState -> RoomState
changeRoomStateEvents es rs = List.foldr (changeRoomStateEvent) rs es

changeRoomState : JoinedRoom -> RoomState -> RoomState
changeRoomState jr rs =
    let
        stateDiff = jr.state
            |> Maybe.andThen .events
            |> Maybe.withDefault []
        timelineDiff = jr.timeline
            |> Maybe.andThen .events
            |> Maybe.map (List.filterMap toStateEvent)
            |> Maybe.withDefault []
    in
        rs
            |> changeRoomStateEvents stateDiff
            |> changeRoomStateEvents timelineDiff

changeTimeline : JoinedRoom -> List (MessageEvent) -> List (MessageEvent)
changeTimeline jr tl =
    let
        newMessages = jr.timeline
            |> Maybe.andThen .events
            |> Maybe.map (List.filterMap toMessageEvent)
            |> Maybe.withDefault []
    in
        tl ++ newMessages

changeEphemeral : JoinedRoom -> Ephemeral -> Ephemeral
changeEphemeral jr e = Maybe.withDefault e jr.ephemeral

changeNotifications : JoinedRoom -> UnreadNotificationCounts -> UnreadNotificationCounts
changeNotifications jr un = Maybe.withDefault un jr.unreadNotifications

changeRoomData : JoinedRoom -> RoomData -> RoomData
changeRoomData jr rd =
    { rd | accountData = applyAccountData jr.accountData rd.accountData
    , roomState = changeRoomState jr rd.roomState
    , messages = changeTimeline jr rd.messages
    , ephemeral = changeEphemeral jr rd.ephemeral
    , unreadNotifications = changeNotifications jr rd.unreadNotifications
    , prevHistoryBatch =
        case rd.prevHistoryBatch of
            Nothing -> Maybe.andThen .prevBatch jr.timeline
            Just _ -> rd.prevHistoryBatch
    }

updateRoomData : JoinedRoom -> Maybe RoomData -> Maybe RoomData
updateRoomData jr mrd = Maybe.withDefault emptyRoomData mrd
    |> changeRoomData jr
    |> Just

applyJoinedRoom : RoomId -> JoinedRoom -> OpenRooms -> OpenRooms
applyJoinedRoom rid jr = Dict.update rid (updateRoomData jr)

applySync : SyncResponse -> OpenRooms -> OpenRooms
applySync sr or =
    let
        joinedRooms = sr.rooms
            |> Maybe.andThen .join
            |> Maybe.withDefault Dict.empty
    in
        Dict.foldl applyJoinedRoom or joinedRooms

addHistoryRoomData : HistoryResponse -> Maybe RoomData -> Maybe RoomData
addHistoryRoomData hr = Maybe.map
    (\rd ->
        { rd | messages = uniqueBy .eventId
            <| (List.reverse <| List.filterMap toMessageEvent hr.chunk) ++ rd.messages
        , prevHistoryBatch = Just hr.end
        })

applyHistoryResponse : RoomId -> HistoryResponse -> OpenRooms -> OpenRooms
applyHistoryResponse rid hr = Dict.update rid (addHistoryRoomData hr)

getStateData : (String, String) -> Decoder a -> RoomData -> Maybe a
getStateData k d rd = Dict.get k rd.roomState
    |> Maybe.andThen (Result.toMaybe << decodeValue d)

getEphemeralData : String -> Decoder a -> RoomData -> Maybe a
getEphemeralData k d rd = rd.ephemeral.events
    |> Maybe.andThen (findFirst ((==) k << .type_))
    |> Maybe.andThen (Result.toMaybe << decodeValue d << .content)

getRoomTypingUsers : RoomData -> List String
getRoomTypingUsers = Maybe.withDefault [] 
    << getEphemeralData "m.typing" (field "user_ids" (list string))

getRoomName : AccountData -> RoomId -> RoomData -> String
getRoomName ad rid rd =
    let
        customName = getStateData ("m.room.name", "") (field "name" (string)) rd
        direct = getDirectMessages ad
            |> Maybe.andThen (Dict.get rid)
    in
        case (customName, direct) of
            (Just cn, _) -> cn
            (_, Just d) -> getLocalDisplayName rd d
            _ -> rid

getLocalDisplayName : RoomData -> Username -> String
getLocalDisplayName rd u =
    getStateData ("m.room.member", u) (field "displayname" string) rd
    |> Maybe.withDefault u

getNotificationCount : RoomData -> (Int, Int)
getNotificationCount rd =
    ( Maybe.withDefault 0 rd.unreadNotifications.notificationCount
    , Maybe.withDefault 0 rd.unreadNotifications.highlightCount
    )

getTotalNotificationCount : OpenRooms -> (Int, Int)
getTotalNotificationCount =
    let
        sumTuples (x1, y1) (x2, y2) = (x1+x2, y1+y2)
    in
        Dict.foldl (\_ -> sumTuples << getNotificationCount) (0, 0)

getTotalNotificationCountString : OpenRooms -> Maybe String
getTotalNotificationCountString or =
    let
        (n, h) = getTotalNotificationCount or
        suffix = case h of
            0 -> ""
            _ -> "!"
    in
        case n of
            0 -> Nothing
            _ -> Just <| "(" ++ String.fromInt n ++ suffix ++ ")"

getHomeserver : String -> String
getHomeserver s =
    let
        colonIndex = Maybe.withDefault 0
            <| Maybe.map ((+) 1)
            <| List.head
            <| String.indexes ":" s
    in
        String.dropLeft colonIndex s

