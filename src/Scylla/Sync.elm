module Scylla.Sync exposing (..)
import Scylla.Api exposing (..)
import Scylla.Login exposing (Username)
import Scylla.Route exposing (RoomId)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, int, string, float, list, value, dict, bool, field)
import Json.Decode.Pipeline exposing (required, optional)
import Set exposing (Set)

-- Special Decoding
decodeJust : Decoder a -> Decoder (Maybe a)
decodeJust = Decode.map Just

maybeDecode : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
maybeDecode s d = optional s (decodeJust d) Nothing

-- General Events
type alias Event =
    { content : Decode.Value
    , type_ : String
    }

eventDecoder : Decoder Event
eventDecoder =
    Decode.succeed Event
        |> required "content" value
        |> required "type" string

type alias EventContent =
    { avatarUrl : Maybe String
    , displayname : Maybe String
    , membership : String
    , isDirect : Maybe Bool
    -- , thirdPartyInvite : Invite
    , unsigned : Maybe UnsignedData
    }

eventContentDecoder : Decoder EventContent
eventContentDecoder =
    Decode.succeed EventContent
        |> maybeDecode "avatar_url" string
        |> maybeDecode "displayname" string
        |> required "membership" string
        |> maybeDecode "is_direct" bool
        -- |> required "third_party_invite" inviteDecoder
        |> maybeDecode "unsigned" unsignedDataDecoder

-- Unsigned Data
type alias UnsignedData =
    { age : Maybe Int
    , redactedBecause : Maybe Event
    , transactionId : Maybe String
    }

unsignedDataDecoder : Decoder UnsignedData
unsignedDataDecoder =
    Decode.succeed UnsignedData
        |> maybeDecode "age" int
        |> maybeDecode "redacted_because" eventDecoder
        |> maybeDecode "transaction_id" string

-- State
type alias State =
    { events : Maybe (List StateEvent)
    }

stateDecoder : Decoder State
stateDecoder =
    Decode.succeed State
        |> maybeDecode "events" (list stateEventDecoder)

type alias StateEvent =
    { content : Decode.Value
    , type_ : String
    , eventId : String
    , sender : String
    , originServerTs : Int
    , unsigned : Maybe UnsignedData
    , prevContent : Maybe EventContent
    , stateKey : String
    }

stateEventDecoder : Decoder StateEvent
stateEventDecoder =
    Decode.succeed StateEvent
        |> required "content" value
        |> required "type" string
        |> required "event_id" string
        |> required "sender" string
        |> required "origin_server_ts" int
        |> maybeDecode "unsigned" unsignedDataDecoder
        |> maybeDecode "prev_content" eventContentDecoder
        |> required "state_key" string

-- Rooms
type alias Rooms =
    { join : Maybe (Dict String JoinedRoom)
    , invite : Maybe (Dict String InvitedRoom)
    , leave : Maybe (Dict String LeftRoom)
    }

roomsDecoder : Decoder Rooms
roomsDecoder =
    Decode.succeed Rooms
        |> maybeDecode "join" (dict joinedRoomDecoder)
        |> maybeDecode "invite" (dict invitedRoomDecoder)
        |> maybeDecode "leave" (dict leftRoomDecoder)

type alias JoinedRoom =
    { state : Maybe State
    , timeline : Maybe Timeline
    , ephemeral : Maybe Ephemeral
    , accountData : Maybe AccountData
    , unreadNotifications : Maybe UnreadNotificationCounts
    }

joinedRoomDecoder : Decoder JoinedRoom
joinedRoomDecoder =
    Decode.succeed JoinedRoom
        |> maybeDecode "state" stateDecoder
        |> maybeDecode "timeline" timelineDecoder
        |> maybeDecode "ephemeral" ephemeralDecoder
        |> maybeDecode "account_data" accountDataDecoder
        |> maybeDecode "unread_notifications" unreadNotificationCountsDecoder


--  Joined Room Data
type alias Timeline =
    { events : Maybe (List RoomEvent)
    , limited : Maybe Bool
    , prevBatch : Maybe String
    }

timelineDecoder =
    Decode.succeed Timeline
        |> maybeDecode "events" (list roomEventDecoder)
        |> maybeDecode "limited" bool
        |> maybeDecode "prev_batch" string

type alias RoomEvent =
    { content : Decode.Value
    , type_ : String
    , eventId : String
    , sender : String
    , originServerTs : Int
    , unsigned : Maybe UnsignedData
    }

roomEventDecoder : Decoder RoomEvent
roomEventDecoder =
    Decode.succeed RoomEvent
        |> required "content" value
        |> required "type" string
        |> required "event_id" string
        |> required "sender" string
        |> required "origin_server_ts" int
        |> maybeDecode "unsigned" unsignedDataDecoder

type alias Ephemeral =
    { events : Maybe (List Event)
    }

ephemeralDecoder : Decoder Ephemeral
ephemeralDecoder =
    Decode.succeed Ephemeral
        |> maybeDecode "events" (list eventDecoder)

type alias AccountData =
    { events : Maybe (List Event)
    }

accountDataDecoder : Decoder AccountData
accountDataDecoder =
    Decode.succeed AccountData
        |> maybeDecode "events" (list eventDecoder)

type alias UnreadNotificationCounts =
    { highlightCount : Maybe Int
    , notificationCount : Maybe Int
    }

unreadNotificationCountsDecoder : Decoder UnreadNotificationCounts
unreadNotificationCountsDecoder =
    Decode.succeed UnreadNotificationCounts
        |> maybeDecode "highlight_count" int
        |> maybeDecode "notification_count" int

--  Invited Room Data
type alias InvitedRoom =
    { inviteState : Maybe InviteState
    }

invitedRoomDecoder : Decoder InvitedRoom
invitedRoomDecoder =
    Decode.succeed InvitedRoom
        |> maybeDecode "invite_state" inviteStateDecoder

type alias InviteState =
    { events : Maybe (List StrippedState)
    }

inviteStateDecoder : Decoder InviteState
inviteStateDecoder =
    Decode.succeed InviteState
        |> maybeDecode "events" (list strippedStateDecoder)

type alias StrippedState =
    { content : EventContent
    , stateKey : String
    , type_ : String
    , sender : String
    }

strippedStateDecoder : Decoder StrippedState
strippedStateDecoder =
    Decode.succeed StrippedState
        |> required "content" eventContentDecoder
        |> required "state_key" string
        |> required "type" string
        |> required "sender" string

-- Left Room Data
type alias LeftRoom =
    { state : Maybe State
    , timeline : Maybe Timeline
    , accountData : Maybe AccountData
    }

leftRoomDecoder : Decoder LeftRoom
leftRoomDecoder =
    Decode.succeed LeftRoom
        |> maybeDecode "state" stateDecoder
        |> maybeDecode "timeline" timelineDecoder
        |> maybeDecode "account_data" accountDataDecoder

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
groupBy : (a -> comparable) -> List a -> Dict comparable (List a)
groupBy f xs =
    let
        update v ml = case ml of
            Just l -> Just (v::l)
            Nothing -> Just [ v ]
    in
        List.foldl (\v acc -> Dict.update (f v) (update v) acc) Dict.empty xs

uniqueByRecursive : (a -> comparable) -> List a -> Set comparable -> List a
uniqueByRecursive f l s = case l of
    x::tail -> if Set.member (f x) s
        then uniqueByRecursive f tail s
        else x::uniqueByRecursive f tail (Set.insert (f x) s)
    [] -> []

uniqueBy : (a -> comparable) -> List a -> List a
uniqueBy f l = uniqueByRecursive f l Set.empty

findFirst : (a -> Bool) -> List a -> Maybe a
findFirst cond l = case l of
    x::xs -> if cond x then Just x else findFirst cond xs
    [] -> Nothing

findLast : (a -> Bool) -> List a -> Maybe a
findLast cond l = findFirst cond <| List.reverse l

findFirstBy : (a -> comparable) -> (a -> Bool) -> List a -> Maybe a
findFirstBy sortFunction cond l = findFirst cond <| List.sortBy sortFunction l

findLastBy : (a -> comparable) -> (a -> Bool) -> List a -> Maybe a
findLastBy sortFunction cond l = findLast cond <| List.sortBy sortFunction l

findFirstEvent : ({ a | originServerTs : Int } -> Bool) -> List { a | originServerTs : Int } -> Maybe { a | originServerTs : Int }
findFirstEvent = findFirstBy .originServerTs

findLastEvent : ({ a | originServerTs : Int } -> Bool) -> List { a | originServerTs : Int } -> Maybe { a | originServerTs : Int }
findLastEvent = findLastBy .originServerTs

-- Business Logic: Merging
mergeMaybe : (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
mergeMaybe f l r = case (l, r) of
    (Just v1, Just v2) -> Just <| f v1 v2
    (Just v, Nothing) -> Just v
    (Nothing, Just v) -> Just v
    _ -> Nothing

mergeEvents : List Event -> List Event -> List Event
mergeEvents l1 l2 = l1 ++ l2

mergeStateEvents : List StateEvent -> List StateEvent -> List StateEvent
mergeStateEvents l1 l2 = uniqueBy .eventId <| l1 ++ l2

mergeRoomEvents : List RoomEvent -> List RoomEvent -> List RoomEvent
mergeRoomEvents l1 l2 = uniqueBy .eventId <| l1 ++ l2

mergeStrippedStates : List StrippedState -> List StrippedState -> List StrippedState
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
senderName : String -> String
senderName s =
    let
        colonIndex = Maybe.withDefault -1 
            <| List.head
            <| String.indexes ":" s
    in
        String.slice 1 colonIndex s

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
        roomToStateEvent re =
            { content = re.content
            , type_ = re.type_
            , eventId = re.eventId
            , sender = re.sender
            , originServerTs = re.originServerTs
            , unsigned = re.unsigned
            , prevContent = Nothing
            , stateKey = ""
            }
        allStateEvents = uniqueBy .eventId (stateEvents ++ (List.map roomToStateEvent timelineEvents))
    in
        allStateEvents

allRoomDictTimelineEvents : Dict String { a | timeline : Maybe Timeline } -> List RoomEvent
allRoomDictTimelineEvents dict = List.concatMap (Maybe.withDefault [] << .events)
    <| List.filterMap .timeline
    <| Dict.values dict

allTimelineEvents : SyncResponse -> List RoomEvent
allTimelineEvents s =
    let
        eventsFor f = Maybe.withDefault []
            <| Maybe.map allRoomDictTimelineEvents
            <| Maybe.andThen f s.rooms
        joinedEvents = eventsFor .join
        leftEvents = eventsFor .leave
    in
        uniqueBy .eventId <| leftEvents ++ joinedEvents

joinedRoomsTimelineEvents : SyncResponse -> Dict String (List RoomEvent)
joinedRoomsTimelineEvents s =
    Maybe.withDefault Dict.empty
    <| Maybe.map (Dict.map (\k v -> Maybe.withDefault [] <| Maybe.andThen .events v.timeline))
    <| Maybe.andThen .join s.rooms

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
allUsers s = uniqueBy (\u -> u) <| List.map .sender <| allTimelineEvents s
