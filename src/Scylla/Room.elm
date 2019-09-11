module Scylla.Room exposing (..)
import Scylla.Route exposing (RoomId)
import Scylla.Sync exposing (SyncResponse)
import Scylla.Sync.Events exposing (MessageEvent, StateEvent, toStateEvent, toMessageEvent)
import Scylla.Sync.AccountData exposing (AccountData)
import Scylla.Sync.Rooms exposing (JoinedRoom, UnreadNotificationCounts, Ephemeral)
import Json.Decode as Decode exposing (Value)
import Dict exposing (Dict)

type alias RoomState = Dict (String, String) Value

type alias RoomData =
    { roomState : RoomState
    , messages : List (MessageEvent)
    , accountData : AccountData
    , ephemeral : Ephemeral
    , unreadNotifications : UnreadNotificationCounts
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

changeAccountData : JoinedRoom -> AccountData -> AccountData
changeAccountData jr ad =
    case jr.accountData of
        Nothing -> ad
        Just newAd ->
            case (newAd.events, ad.events) of
                (Just es, Nothing) -> newAd
                (Just newEs, Just es) -> { events = Just (newEs ++ es) }
                _ -> ad

changeTimeline : JoinedRoom -> List (MessageEvent) -> List (MessageEvent)
changeTimeline jr tl =
    let
        newMessages = jr.timeline
            |> Maybe.andThen .events
            |> Maybe.map (List.filterMap toMessageEvent)
            |> Maybe.withDefault []
    in
        newMessages ++ tl

changeEphemeral : JoinedRoom -> Ephemeral -> Ephemeral
changeEphemeral jr e = Maybe.withDefault e jr.ephemeral

changeNotifications : JoinedRoom -> UnreadNotificationCounts -> UnreadNotificationCounts
changeNotifications jr un = Maybe.withDefault un jr.unreadNotifications

changeRoomData : JoinedRoom -> RoomData -> RoomData
changeRoomData jr rd =
    { rd | accountData = changeAccountData jr rd.accountData
    , roomState = changeRoomState jr rd.roomState
    , messages = changeTimeline jr rd.messages
    , ephemeral = changeEphemeral jr rd.ephemeral
    , unreadNotifications = changeNotifications jr rd.unreadNotifications
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
