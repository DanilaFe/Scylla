module Scylla.Sync.Rooms exposing (..)
import Scylla.Sync.DecodeTools exposing (maybeDecode)
import Scylla.Sync.Events exposing (Event, RoomEvent, StateEvent, StrippedStateEvent, stateEventDecoder, strippedStateEventDecoder, roomEventDecoder, eventDecoder)
import Scylla.Sync.AccountData exposing (AccountData, accountDataDecoder)
import Json.Decode as Decode exposing (Decoder, int, string, dict, list, bool)
import Json.Decode.Pipeline exposing (required)
import Dict exposing (Dict)

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

type alias InvitedRoom =
    { inviteState : Maybe InviteState
    }

invitedRoomDecoder : Decoder InvitedRoom
invitedRoomDecoder =
    Decode.succeed InvitedRoom
        |> maybeDecode "invite_state" inviteStateDecoder

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

type alias State =
    { events : Maybe (List StateEvent)
    }

stateDecoder : Decoder State
stateDecoder =
    Decode.succeed State
        |> maybeDecode "events" (list stateEventDecoder)

type alias InviteState =
    { events : Maybe (List StrippedStateEvent)
    }

inviteStateDecoder : Decoder InviteState
inviteStateDecoder =
    Decode.succeed InviteState
        |> maybeDecode "events" (list strippedStateEventDecoder)

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

type alias Ephemeral =
    { events : Maybe (List Event)
    }

ephemeralDecoder : Decoder Ephemeral
ephemeralDecoder =
    Decode.succeed Ephemeral
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
