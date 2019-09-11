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

-- Business Logic: Events
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

-- Business Logic: Users
allUsers : SyncResponse -> List Username
allUsers s = uniqueBy (\u -> u) <| List.map getSender <| allTimelineEvents s
