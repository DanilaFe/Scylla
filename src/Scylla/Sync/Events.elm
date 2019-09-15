module Scylla.Sync.Events exposing (..)
import Scylla.Sync.DecodeTools exposing (maybeDecode)
import Json.Decode as Decode exposing (Decoder, int, string, value, oneOf)
import Json.Decode.Pipeline exposing (required)

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

type alias EventContent = Decode.Value

eventContentDecoder : Decoder EventContent
eventContentDecoder = Decode.value

type alias Event =
    { content : Decode.Value
    , type_ : String
    }

eventDecoder : Decoder Event
eventDecoder =
    Decode.succeed Event
        |> required "content" value
        |> required "type" string

type RoomEvent
    = StateRoomEvent StateEvent
    | MessageRoomEvent MessageEvent

roomEventDecoder : Decoder RoomEvent
roomEventDecoder = oneOf
    [ Decode.map StateRoomEvent stateEventDecoder
    , Decode.map MessageRoomEvent messageEventDecoder
    ]

type alias MessageEvent =
    { content : EventContent
    , type_ : String
    , eventId : String
    , sender : String
    , originServerTs : Int
    , unsigned : Maybe UnsignedData
    }

messageEventDecoder : Decoder MessageEvent
messageEventDecoder =
    Decode.succeed MessageEvent
        |> required "content" value
        |> required "type" string
        |> required "event_id" string
        |> required "sender" string
        |> required "origin_server_ts" int
        |> maybeDecode "unsigned" unsignedDataDecoder

type alias StateEvent =
    { content : EventContent
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

type alias StrippedStateEvent =
    { content : EventContent
    , stateKey : String
    , type_ : String
    , sender : String
    }

strippedStateEventDecoder : Decoder StrippedStateEvent
strippedStateEventDecoder =
    Decode.succeed StrippedStateEvent
        |> required "content" eventContentDecoder
        |> required "state_key" string
        |> required "type" string
        |> required "sender" string

-- Operations on Room Events
getUnsigned : RoomEvent -> Maybe UnsignedData
getUnsigned re =
    case re of
        StateRoomEvent e -> e.unsigned
        MessageRoomEvent e -> e.unsigned

getEventId : RoomEvent -> String
getEventId re =
    case re of
        StateRoomEvent e -> e.eventId
        MessageRoomEvent e -> e.eventId

getSender : RoomEvent -> String
getSender re =
    case re of
        StateRoomEvent e -> e.sender
        MessageRoomEvent e -> e.sender

getType : RoomEvent -> String
getType re =
    case re of
        StateRoomEvent e -> e.type_
        MessageRoomEvent e -> e.type_

toStateEvent : RoomEvent -> Maybe StateEvent
toStateEvent re =
    case re of
        StateRoomEvent e -> Just e
        _ -> Nothing

toMessageEvent : RoomEvent -> Maybe MessageEvent
toMessageEvent re =
    case re of
        MessageRoomEvent e -> Just e
        _ -> Nothing

toEvent : RoomEvent -> Event
toEvent re =
    case re of
        StateRoomEvent e -> { content = e.content, type_ = e.type_ }
        MessageRoomEvent e -> { content = e.content, type_ = e.type_ }
