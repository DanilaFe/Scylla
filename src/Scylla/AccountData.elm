module Scylla.AccountData exposing (..)
import Scylla.Sync exposing (SyncResponse, AccountData, JoinedRoom, roomAccountData)
import Json.Decode as Decode
import Dict

type NotificationSetting = Normal | MentionsOnly | None

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

