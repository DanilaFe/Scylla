module Scylla.Sync.Push exposing (..)
import Scylla.Sync.DecodeTools exposing (maybeDecode)
import Scylla.Sync.Events exposing (RoomEvent, getSender, getContent, getType)
import Scylla.Route exposing (RoomId)
import Json.Decode as Decode exposing (Decoder, string, int, field, value, bool, list)
import Json.Decode.Pipeline exposing (required, optional)

type Condition
    = EventMatch String String
    | ContainsDisplayName
    | RoomMemberCount Int
    | SenderNotificationPermission String

conditionDecoder : Decoder Condition
conditionDecoder =
    let
        eventMatchDecoder =
            Decode.succeed EventMatch
            |> required "key" string
            |> required "pattern" string
        containsDisplayNameDecoder =
            Decode.succeed ContainsDisplayName
        roomMemberCountDecoder =
            Decode.succeed RoomMemberCount
            |> required "is" 
                (Decode.map (Maybe.withDefault 0 << String.toInt) string)
        senderNotifPermissionDecoder =
            Decode.succeed SenderNotificationPermission
            |> required "key" string
        dispatchDecoder k =
            case k of
                "event_match" -> eventMatchDecoder
                "contains_display_name" -> containsDisplayNameDecoder
                "room_member_count" -> roomMemberCountDecoder
                "sender_notification_permission" -> senderNotifPermissionDecoder
                _ -> Decode.fail "Unknown condition code"
    in
        field "kind" string
        |> Decode.andThen dispatchDecoder

type Action
    = Notify
    | DontNotify
    | Coalesce
    | SetTweak String (Maybe Decode.Value)

actionDecoder : Decoder Action
actionDecoder =
    let
        dispatchStringDecoder s =
            case s of
                "notify" -> Decode.succeed Notify
                "dont_notify" -> Decode.succeed DontNotify
                "coalesce" -> Decode.succeed Coalesce
                _ -> Decode.fail "Unknown action string"
        objectDecoder =
            Decode.succeed SetTweak
            |> required "set_tweak" string
            |> maybeDecode "value" value
    in
        Decode.oneOf
            [ string |> Decode.andThen dispatchStringDecoder
            , objectDecoder
            ]

type alias Rule =
    { ruleId : String
    , default : Bool
    , enabled : Bool
    , conditions : List Condition
    , actions : List Action
    }

ruleDecoder : Decoder Rule
ruleDecoder =
    let
        patternDecoder = Decode.oneOf
            [ field "pattern" string
                |> Decode.andThen
                    (\p -> Decode.succeed <| \r ->
                        { r | conditions = (EventMatch "content.body" p)::r.conditions })
            , Decode.succeed identity
            ]
        basicRuleDecoder = Decode.succeed Rule
            |> required "rule_id" string
            |> optional "default" bool True
            |> optional "enabled" bool False
            |> optional "conditions" (list conditionDecoder) []
            |> required "actions" (list actionDecoder)
    in
        patternDecoder
        |> Decode.andThen (\f -> Decode.map f basicRuleDecoder)

type alias Ruleset =
    { content : List Rule
    , override : List Rule
    , room : List Rule
    , sender : List Rule
    , underride : List Rule
    }

rulesetDecoder : Decoder Ruleset
rulesetDecoder = Decode.succeed Ruleset
    |> optional "content" (list ruleDecoder) []
    |> optional "override" (list ruleDecoder) []
    |> optional "room" (list ruleDecoder) []
    |> optional "sender" (list ruleDecoder) []
    |> optional "underride" (list ruleDecoder) []

checkCondition : RoomEvent -> Condition -> Bool
checkCondition re c =
    let
        pathDecoder xs p = 
            Decode.at xs string 
            |> Decode.map (String.contains p << String.toLower) 
        matchesPattern xs p =
            case Decode.decodeValue (pathDecoder xs p) (getContent re) of
                Ok True -> True
                _ -> False
    in
        case c of
            EventMatch k p ->
                case String.split "." k of
                    "content"::xs -> matchesPattern xs p
                    "type"::[] -> String.contains p <| getType re
                    _ -> False
            ContainsDisplayName -> False
            RoomMemberCount _ -> False
            SenderNotificationPermission _ -> False

applyAction : Action -> List Action -> List Action
applyAction a as_ =
    case a of
        Notify -> Notify :: List.filter (\a_ -> a_ /= DontNotify) as_
        DontNotify -> DontNotify :: List.filter (\a_ -> a_ /= Notify) as_
        Coalesce -> Coalesce :: List.filter (\a_ -> a_ /= DontNotify) as_
        a_ -> a_ :: as_

applyActions : List Action -> List Action -> List Action
applyActions l r = List.foldl applyAction r l

updatePushRuleActions : Rule -> RoomEvent -> List Action -> List Action
updatePushRuleActions r re as_ =
    if List.all (checkCondition re) r.conditions
    then applyActions r.actions as_
    else as_

updatePushActions : List Rule -> RoomEvent -> List Action -> List Action
updatePushActions rs re as_ =
    List.filter .enabled rs
    |> List.foldl (\r -> updatePushRuleActions r re) as_

getPushActions : Ruleset -> RoomId -> RoomEvent -> List Action
getPushActions rs rid re =
    let
        roomRules = List.filter (((==) rid) << .ruleId) rs.room
        senderRules = List.filter (((==) <| getSender re) << .ruleId) rs.sender
    in
        updatePushActions rs.underride re []
        |> updatePushActions senderRules re
        |> updatePushActions roomRules re
        |> updatePushActions rs.override re

getEventNotification : Ruleset -> RoomId -> RoomEvent -> Bool
getEventNotification rs rid re =
    getPushActions rs rid re
    |> List.member Notify
