module Scylla.Sync exposing (..)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, int, string, float, list, value, dict, bool)
import Json.Decode.Pipeline exposing (required, optional)

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
    { avatarUrl : String
    , displayname : String
    , membership : String
    , isDirect : Bool
    -- , thirdPartyInvite : Invite
    , unsigned : UnsignedData
    }

eventContentDecoder : Decoder EventContent
eventContentDecoder =
    Decode.succeed EventContent
        |> required "avatar_url" string
        |> required "displayname" string
        |> required "membership" string
        |> required "is_direct" bool
        -- |> required "third_party_invite" inviteDecoder
        |> required "unsigned" unsignedDataDecoder

type alias State =
    { events : List StateEvent
    }

-- Unsigned Data
type alias UnsignedData =
    { age : Int
    , redactedBecause : Event
    , transactionId : String
    }

unsignedDataDecoder : Decoder UnsignedData
unsignedDataDecoder =
    Decode.succeed UnsignedData
        |> required "age" int
        |> required "redacted_because" eventDecoder
        |> required "transaction_id" string

-- State
stateDecoder : Decoder State
stateDecoder =
    Decode.succeed State
        |> required "events" (list stateEventDecoder)

type alias StateEvent =
    { content : Decode.Value
    , type_ : String
    , eventId : String
    , sender : String
    , originServerTs : Int
    , unsigned : UnsignedData
    , prevContent : EventContent
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
        |> required "unsigned" unsignedDataDecoder
        |> required "prev_content" eventContentDecoder
        |> required "sate_key" string

-- Rooms
type alias Rooms =
    { join : Dict String JoinedRoom
    , invite : Dict String InvitedRoom
    , leave : Dict String LeftRoom
    }

roomsDecoder : Decoder Rooms
roomsDecoder =
    Decode.succeed Rooms
        |> required "join" (dict joinedRoomDecoder)
        |> required "invite" (dict invitedRoomDecoder)
        |> required "leave" (dict leftRoomDecoder)

type alias JoinedRoom =
    { state : State
    , timeline : Timeline
    , ephemeral : Ephemeral
    , accountData : AccountData
    , unreadNotifications : UnreadNotificationCounts
    }

joinedRoomDecoder : Decoder JoinedRoom
joinedRoomDecoder =
    Decode.succeed JoinedRoom
        |> required "state" stateDecoder
        |> required "timeline" timelineDecoder
        |> required "ephemeral" ephemeralDecoder
        |> required "account_data" accountDataDecoder
        |> required "unread_notifications" unreadNotificationCountsDecoder


--  Joined Room Data
type alias Timeline =
    { events : List RoomEvent
    , limited : Bool
    , prevBatch : String
    }

timelineDecoder =
    Decode.succeed Timeline
        |> required "events" (list roomEventDecoder)
        |> required "limited" bool
        |> required "prev_batch" string

type alias RoomEvent =
    { content : Decode.Value
    , type_ : String
    , eventId : String
    , sender : String
    , originServerTs : Int
    , unsigned : UnsignedData
    }

roomEventDecoder : Decoder RoomEvent
roomEventDecoder =
    Decode.succeed RoomEvent
        |> required "content" value
        |> required "type" string
        |> required "event_id" string
        |> required "sender" string
        |> required "origin_server_ts" int
        |> required "unsigned" unsignedDataDecoder

type alias Ephemeral =
    { events : List Event
    }

ephemeralDecoder : Decoder Ephemeral
ephemeralDecoder =
    Decode.succeed Ephemeral
        |> required "events" (list eventDecoder)

type alias AccountData =
    { events : List Event
    }

accountDataDecoder : Decoder AccountData
accountDataDecoder =
    Decode.succeed AccountData
        |> required "events" (list eventDecoder)

type alias UnreadNotificationCounts =
    { highlightCount : Int
    , notificationCount : Int
    }

unreadNotificationCountsDecoder : Decoder UnreadNotificationCounts
unreadNotificationCountsDecoder =
    Decode.succeed UnreadNotificationCounts
        |> required "highlight_count" int
        |> required "notification_count" int

--  Invited Room Data
type alias InvitedRoom =
    { inviteState : InviteState
    }

invitedRoomDecoder : Decoder InvitedRoom
invitedRoomDecoder =
    Decode.succeed InvitedRoom
        |> required "invite_state" inviteStateDecoder

type alias InviteState =
    { events : List StrippedState
    }

inviteStateDecoder : Decoder InviteState
inviteStateDecoder =
    Decode.succeed InviteState
        |> required "events" (list strippedStateDecoder)

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
    { state : State
    , timeline : Timeline
    , accountData : AccountData
    }

leftRoomDecoder : Decoder LeftRoom
leftRoomDecoder =
    Decode.succeed LeftRoom
        |> required "state" stateDecoder
        |> required "timeline" timelineDecoder
        |> required "account_data" accountDataDecoder
