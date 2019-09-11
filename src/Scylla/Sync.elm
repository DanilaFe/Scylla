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

-- Business Logic: Merging
mergeMaybe : (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
mergeMaybe f l r = case (l, r) of
    (Just v1, Just v2) -> Just <| f v1 v2
    (Just v, Nothing) -> l
    (Nothing, Just v) -> r
    _ -> Nothing

mergeEvents : List Event -> List Event -> List Event
mergeEvents l1 l2 = l1 ++ l2

mergeStateEvents : List StateEvent -> List StateEvent -> List StateEvent
mergeStateEvents l1 l2 = uniqueBy .eventId <| l1 ++ l2

mergeRoomEvents : List RoomEvent -> List RoomEvent -> List RoomEvent
mergeRoomEvents l1 l2 = uniqueBy getEventId <| l1 ++ l2

mergeStrippedStates : List StrippedStateEvent -> List StrippedStateEvent -> List StrippedStateEvent
mergeStrippedStates l1 l2 = l1 ++ l2

mergeAccountData : AccountData -> AccountData -> AccountData
mergeAccountData a1 a2 = AccountData <| mergeMaybe mergeEvents a1.events a2.events

mergePresence : Presence -> Presence -> Presence
mergePresence p1 p2 = Presence <| mergeMaybe mergeEvents p1.events p2.events

mergeDicts : (b -> b -> b) -> Dict comparable b -> Dict comparable b -> Dict comparable b
mergeDicts f d1 d2 =
    let
        inOne = Dict.insert
        inBoth k v1 v2 = Dict.insert k (f v1 v2)
    in
        Dict.merge inOne inBoth inOne d1 d2 (Dict.empty)

mergeState : State -> State -> State
mergeState s1 s2 = State <| mergeMaybe mergeStateEvents s1.events s2.events

mergeTimeline : Timeline -> Timeline -> Timeline
mergeTimeline t1 t2 = Timeline (mergeMaybe mergeRoomEvents t1.events t2.events) Nothing t1.prevBatch

mergeEphemeral : Ephemeral -> Ephemeral -> Ephemeral
mergeEphemeral e1 e2 = Ephemeral <| mergeMaybe mergeEvents e1.events e2.events

mergeJoinedRoom : JoinedRoom -> JoinedRoom -> JoinedRoom
mergeJoinedRoom r1 r2 =
    { r2 | state = mergeMaybe mergeState r1.state r2.state
    , timeline = mergeMaybe mergeTimeline r1.timeline r2.timeline
    , accountData = mergeMaybe mergeAccountData r1.accountData r2.accountData
    , ephemeral = mergeMaybe mergeEphemeral r1.ephemeral r2.ephemeral
    }

mergeInviteState : InviteState -> InviteState -> InviteState 
mergeInviteState i1 i2 = InviteState <| mergeMaybe mergeStrippedStates i1.events i2.events

mergeInvitedRoom : InvitedRoom -> InvitedRoom -> InvitedRoom
mergeInvitedRoom i1 i2 = InvitedRoom <| mergeMaybe mergeInviteState i1.inviteState i2.inviteState

mergeLeftRoom : LeftRoom -> LeftRoom -> LeftRoom
mergeLeftRoom l1 l2 = LeftRoom
    (mergeMaybe mergeState l1.state l2.state)
    (mergeMaybe mergeTimeline l1.timeline l2.timeline)
    (mergeMaybe mergeAccountData l1.accountData l2.accountData)

mergeJoin : Dict String JoinedRoom -> Dict String JoinedRoom -> Dict String JoinedRoom
mergeJoin = mergeDicts mergeJoinedRoom

mergeInvite : Dict String InvitedRoom -> Dict String InvitedRoom -> Dict String InvitedRoom
mergeInvite = mergeDicts mergeInvitedRoom

mergeLeave : Dict String LeftRoom -> Dict String LeftRoom -> Dict String LeftRoom
mergeLeave = mergeDicts mergeLeftRoom

mergeRooms : Rooms -> Rooms -> Rooms
mergeRooms r1 r2 =
    { join = mergeMaybe mergeJoin r1.join r2.join
    , invite = mergeMaybe mergeInvite r1.invite r2.invite
    , leave = mergeMaybe mergeLeave r1.leave r2.leave
    }

mergeSyncResponse : SyncResponse -> SyncResponse -> SyncResponse
mergeSyncResponse l r = 
    { r | rooms = mergeMaybe mergeRooms l.rooms r.rooms
    , accountData = mergeMaybe mergeAccountData l.accountData r.accountData
    }

appendRoomHistoryResponse : JoinedRoom -> HistoryResponse -> JoinedRoom
appendRoomHistoryResponse jr hr =
    let
        oldEvents = Maybe.withDefault [] <| Maybe.andThen .events jr.timeline
        newEvents = mergeRoomEvents (List.reverse hr.chunk) oldEvents
        newTimeline = case jr.timeline of
            Just t -> Just { t | events = Just newEvents, prevBatch = Just hr.end }
            Nothing -> Just { events = Just newEvents, prevBatch = Just hr.end, limited = Nothing }
    in
        { jr | timeline = newTimeline }

appendHistoryResponse : SyncResponse -> RoomId -> HistoryResponse -> SyncResponse
appendHistoryResponse sr r hr =
    let
        appendMaybeRoomHistoryResponse mr = Just <| case mr of
            Just jr -> appendRoomHistoryResponse jr hr
            Nothing ->
                { state = Nothing
                , timeline = Just
                    { events = Just hr.chunk
                    , limited = Nothing
                    , prevBatch = Just hr.end
                    }
                , ephemeral = Nothing
                , accountData = Nothing
                , unreadNotifications = Nothing
                }
        newRooms = Just <| case sr.rooms of
            Just rs -> { rs | join = newJoin rs.join }
            Nothing -> { join = newJoin Nothing, leave = Nothing, invite = Nothing }
        newJoin j = Maybe.map (Dict.update r appendMaybeRoomHistoryResponse) j
    in
        { sr | rooms = newRooms }

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
