module Scylla.Sync exposing (..)
import Scylla.Api exposing (..)
import Scylla.Login exposing (Username)
import Scylla.Route exposing (RoomId)
import Scylla.ListUtils exposing (..)
import Scylla.Sync.DecodeTools exposing (maybeDecode)
import Scylla.Sync.Events exposing (..)
import Scylla.Sync.Rooms exposing (..)
import Scylla.Sync.AccountData exposing (..)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, int, string, float, list, value, dict, bool, field)
import Json.Decode.Pipeline exposing (required, optional)
import Set exposing (Set)

-- General Sync Response
type alias SyncResponse =
    { nextBatch : String
    , rooms : Maybe Rooms
    , presence : Maybe Presence
    , accountData : Maybe AccountData
    }

syncResponseDecoder : Decoder SyncResponse
syncResponseDecoder =
    Decode.succeed SyncResponse
        |> required "next_batch" string
        |> maybeDecode "rooms" roomsDecoder
        |> maybeDecode "presence" presenceDecoder
        |> maybeDecode "account_data" accountDataDecoder

type alias Presence =
    { events : Maybe (List Event)
    }

presenceDecoder : Decoder Presence
presenceDecoder =
    Decode.succeed Presence
        |> maybeDecode "events" (list eventDecoder)

-- Room History Responses
type alias HistoryResponse =
    { start : String
    , end : String
    , chunk : List RoomEvent
    }

historyResponseDecoder : Decoder HistoryResponse
historyResponseDecoder =
    Decode.succeed HistoryResponse
        |> required "start" string
        |> required "end" string
        |> required "chunk" (list roomEventDecoder)

-- Business Logic: Helper Functions
findFirstEvent : ({ a | originServerTs : Int } -> Bool) -> List { a | originServerTs : Int } -> Maybe { a | originServerTs : Int }
findFirstEvent = findFirstBy .originServerTs

findLastEvent : ({ a | originServerTs : Int } -> Bool) -> List { a | originServerTs : Int } -> Maybe { a | originServerTs : Int }
findLastEvent = findLastBy .originServerTs

-- Business Logic: Names
homeserver : String -> String
homeserver s =
    let
        colonIndex = Maybe.withDefault 0
            <| Maybe.map ((+) 1)
            <| List.head
            <| String.indexes ":" s
    in
        String.dropLeft colonIndex s

-- Business Logic: Events
allRoomStateEvents : JoinedRoom -> List StateEvent
allRoomStateEvents jr =
    let
        stateEvents = Maybe.withDefault [] <|  Maybe.andThen .events jr.state
        timelineEvents = Maybe.withDefault [] <| Maybe.andThen .events jr.timeline
        allStateEvents = uniqueBy .eventId (stateEvents ++ (List.filterMap toStateEvent timelineEvents))
    in
        allStateEvents

allRoomDictTimelineEvents : Dict String { a | timeline : Maybe Timeline } -> List RoomEvent
allRoomDictTimelineEvents dict = List.concatMap (Maybe.withDefault [] << .events)
    <| List.filterMap .timeline
    <| Dict.values dict

allTimelineEventIds : SyncResponse -> List String
allTimelineEventIds s = List.map getEventId <| allTimelineEvents s

allTimelineEvents : SyncResponse -> List RoomEvent
allTimelineEvents s =
    let
        eventsFor f = Maybe.withDefault []
            <| Maybe.map allRoomDictTimelineEvents
            <| Maybe.andThen f s.rooms
        joinedEvents = eventsFor .join
        leftEvents = eventsFor .leave
    in
        leftEvents ++ joinedEvents

joinedRoomsTimelineEvents : SyncResponse -> Dict String (List RoomEvent)
joinedRoomsTimelineEvents s =
    Maybe.withDefault Dict.empty
    <| Maybe.map (Dict.map (\k v -> Maybe.withDefault [] <| Maybe.andThen .events v.timeline))
    <| Maybe.andThen .join s.rooms

totalNotificationCountString : SyncResponse -> Maybe String
totalNotificationCountString sr =
    let
        (h, n) = totalNotificationCounts sr
        suffix = case h of
            0 -> ""
            _ -> "!"
    in
        case n of
            0 -> Nothing
            _ -> Just <| "(" ++ String.fromInt n ++ suffix ++ ")"

totalNotificationCounts : SyncResponse -> (Int, Int)
totalNotificationCounts sr =
    let
        rooms = Maybe.withDefault []
            <| Maybe.map (Dict.values)
            <| Maybe.andThen (.join) sr.rooms
        zeroDefault = Maybe.withDefault 0
        getCounts = Maybe.map (\cs -> (zeroDefault cs.highlightCount, zeroDefault cs.notificationCount))
            << .unreadNotifications
        sumCounts (h1, n1) (h2, n2) = (h1 + h2, n1 + n2)
    in
        List.foldl sumCounts (0, 0)
            <| List.filterMap getCounts rooms

-- Business Logic: Room Info
roomAccountData : JoinedRoom -> String -> Maybe Decode.Value
roomAccountData jr et =
    Maybe.map .content
    <| Maybe.andThen (List.head << List.filter (((==) et) << .type_))
    <| Maybe.andThen .events jr.accountData

roomName : JoinedRoom -> Maybe String
roomName jr = 
    let
        name c = Result.toMaybe <| Decode.decodeValue (field "name" string) c
        nameEvent = findLastEvent (((==) "m.room.name") << .type_) <| allRoomStateEvents jr
    in
        Maybe.andThen (name << .content) nameEvent

roomTypingUsers : JoinedRoom -> List Username
roomTypingUsers jr = Maybe.withDefault []
    <| Maybe.andThen (Result.toMaybe << Decode.decodeValue (Decode.field "user_ids" (list string)))
    <| Maybe.map .content
    <| Maybe.andThen (findLast (((==) "m.typing") << .type_))
    <| Maybe.andThen .events jr.ephemeral

-- Business Logic: Users
allUsers : SyncResponse -> List Username
allUsers s = uniqueBy (\u -> u) <| List.map getSender <| allTimelineEvents s

roomJoinedUsers : JoinedRoom -> List Username
roomJoinedUsers r = 
    let
        contentDecoder = Decode.field "membership" Decode.string
        isJoin e = Ok "join" == (Decode.decodeValue contentDecoder e.content)
    in
        List.map .sender
        <| List.filter isJoin
        <| List.filter (((==) "m.room.member") << .type_)
        <| allRoomStateEvents r
