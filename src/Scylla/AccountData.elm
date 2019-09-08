module Scylla.AccountData exposing (..)
import Scylla.Sync exposing (SyncResponse, AccountData, JoinedRoom, roomAccountData)
import Json.Decode as Decode
import Json.Encode as Encode
import Dict exposing (Dict)

type NotificationSetting = Normal | MentionsOnly | None

encodeNotificationSetting : NotificationSetting -> Decode.Value
encodeNotificationSetting ns =
    let
        string = case ns of
            Normal -> "Normal"
            MentionsOnly -> "MentionsOnly"
            None -> "None"
    in
        Encode.string string

notificationSettingDecoder : Decode.Decoder NotificationSetting
notificationSettingDecoder =
    let
        checkString s = case s of
            "Normal" -> Decode.succeed Normal
            "MentionsOnly" -> Decode.succeed MentionsOnly
            "None" -> Decode.succeed None
            _ -> Decode.fail "Not a valid notification setting"
    in
        Decode.andThen checkString Decode.string

roomNotificationSetting : JoinedRoom -> NotificationSetting
roomNotificationSetting jr = Maybe.withDefault Normal
    <| Maybe.andThen Result.toMaybe
    <| Maybe.map (Decode.decodeValue notificationSettingDecoder)
    <| roomAccountData jr "com.danilafe.scylla.notifications"

roomIdNotificationSetting : SyncResponse -> String -> NotificationSetting
roomIdNotificationSetting sr s = Maybe.withDefault Normal
    <| Maybe.map roomNotificationSetting
    <| Maybe.andThen (Dict.get s)
    <| Maybe.andThen .join sr.rooms

type alias DirectMessages = Dict String String
type alias DirectMessagesRaw = Dict String (List String)

directMessagesDecoder : Decode.Decoder DirectMessages
directMessagesDecoder =
    Decode.dict (Decode.list Decode.string)
        |> Decode.map (invertDirectMessages)

invertDirectMessages : DirectMessagesRaw -> DirectMessages
invertDirectMessages dmr =
    Dict.foldl
        (\k lv acc -> List.foldl (\v -> Dict.insert v k) acc lv)
        Dict.empty
        dmr

