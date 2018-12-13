module Scylla.Sync exposing (..)
import Scylla.Api exposing (..)
import Scylla.Notification exposing (..)
import Scylla.Login exposing (Username)
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

-- Business Logic
uniqueByRecursive : (a -> comparable) -> List a -> Set comparable -> List a
uniqueByRecursive f l s = case l of
    x::tail -> if Set.member (f x) s
        then uniqueByRecursive f tail s
        else x::uniqueByRecursive f tail (Set.insert (f x) s)
    [] -> []

uniqueBy : (a -> comparable) -> List a -> List a
uniqueBy f l = uniqueByRecursive f l Set.empty

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
mergeTimeline t1 t2 = Timeline (mergeMaybe mergeRoomEvents t1.events t2.events) Nothing t2.prevBatch

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

-- Business Logic: Names
senderName : String -> String
senderName s =
    let
        colonIndex = Maybe.withDefault -1 
            <| List.head
            <| String.indexes ":" s
    in
        String.slice 1 colonIndex s

roomName : JoinedRoom -> Maybe String
roomName jr = 
    let
        state = jr.state
        nameEvent = List.head << List.sortBy (\e -> -e.originServerTs) << List.filter (\e -> e.type_ == "m.room.name")
        name e = Result.toMaybe <| Decode.decodeValue (field "name" string) e.content
    in
        Maybe.andThen name <| Maybe.andThen nameEvent <| Maybe.andThen .events <| state

-- Business Logic: Event Extraction
notificationText : RoomEvent -> String
notificationText re = case (Decode.decodeValue (field "msgtype" string) re.content) of
    Ok "m.text" -> Result.withDefault "" <| (Decode.decodeValue (field "body" string) re.content)
    _ -> ""

notificationEvents : SyncResponse -> List (String, RoomEvent)
notificationEvents s =
    let
        applyPair k = List.map (\v -> (k, v))
    in
        List.sortBy (\(k, v) -> v.originServerTs)
        <| Dict.foldl (\k v a -> a ++ applyPair k v) []
        <| joinedRoomsEvents s

joinedRoomsEvents : SyncResponse -> Dict String (List RoomEvent)
joinedRoomsEvents s =
    Maybe.withDefault Dict.empty
    <| Maybe.map (Dict.map (\k v -> Maybe.withDefault [] <| Maybe.andThen .events v.timeline))
    <| Maybe.andThen .join s.rooms

-- Business Logic: User Extraction
roomsUsers : SyncResponse -> List Username
roomsUsers s =
    let
        users dict =
            List.map .sender
            <| (List.concatMap <| Maybe.withDefault [] << .events)
            <| (List.filterMap .timeline)
            <| Dict.values dict
        usersFor f = Maybe.withDefault [] <| Maybe.map users <| Maybe.andThen f s.rooms
        joinedUsers = usersFor .join
        leftUsers = usersFor .leave
    in
        uniqueBy (\u -> u) <| leftUsers ++ joinedUsers
