module Scylla.Sync exposing (..)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, int, string, float, list, value, dict, bool)
import Json.Decode.Pipeline exposing (required, optional)

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
        |> required "required" value
        |> required "type" string
        |> required "event_id" string
        |> required "sender" string
        |> required "origin_server_ts" int
        |> maybeDecode "unsigned" unsignedDataDecoder
        |> maybeDecode "prev_content" eventContentDecoder
        |> required "sate_key" string

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
