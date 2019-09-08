module Scylla.Model exposing (..)
import Scylla.Api exposing (..)
import Scylla.Sync exposing (SyncResponse, HistoryResponse, JoinedRoom, senderName, roomName, roomJoinedUsers, findFirst, AccountData)
import Scylla.AccountData exposing (directMessagesDecoder)
import Scylla.Login exposing (LoginResponse, Username, Password)
import Scylla.UserData exposing (UserData)
import Scylla.Route exposing (Route(..), RoomId)
import Scylla.Messages exposing (..)
import Scylla.Storage exposing (..)
import Scylla.Markdown exposing (..)
import Browser.Navigation as Nav
import Browser.Dom exposing (Viewport)
import Url.Builder
import Dict exposing (Dict)
import Time exposing (Posix)
import File exposing (File)
import Json.Decode as Decode
import Browser
import Http
import Url exposing (Url)

type alias Model =
    { key : Nav.Key
    , route : Route
    , token : Maybe ApiToken
    , loginUsername : Username
    , loginPassword : Password
    , apiUrl : ApiUrl
    , sync : SyncResponse
    , errors : List String
    , roomText : Dict RoomId String
    , sending : Dict Int (RoomId, SendingMessage)
    , transactionId : Int
    , userData : Dict Username UserData
    , roomNames : Dict RoomId String
    , connected : Bool
    , searchText : String
    }

type Msg =
    ChangeApiUrl ApiUrl -- During login screen: the API URL (homeserver)
    | ChangeLoginUsername Username -- During login screen: the username
    | ChangeLoginPassword Password -- During login screen: the password
    | AttemptLogin -- During login screen, login button presed
    | TryUrl Browser.UrlRequest -- User attempts to change URL
    | OpenRoom String -- We try open a room
    | ChangeRoute Route -- URL changes
    | ChangeRoomText String String -- Change to a room's input text
    | SendRoomText String -- Sends a message typed into a given room's input
    | SendRoomTextResponse Int (Result Http.Error String) -- A send message response finished
    | ViewportAfterMessage (Result Browser.Dom.Error Viewport) -- A message has been received, try scroll (maybe)
    | ViewportChangeComplete (Result Browser.Dom.Error ()) -- We're done changing the viewport.
    | ReceiveFirstSyncResponse (Result Http.Error SyncResponse) -- HTTP, Sync has finished
    | ReceiveSyncResponse (Result Http.Error SyncResponse) -- HTTP, Sync has finished
    | ReceiveLoginResponse ApiUrl (Result Http.Error LoginResponse) -- HTTP, Login has finished
    | ReceiveUserData Username (Result Http.Error UserData) -- HTTP, receive user data
    | ReceiveCompletedReadMarker (Result Http.Error ()) -- HTTP, read marker request completed
    | ReceiveCompletedTypingIndicator (Result Http.Error ()) -- HTTP, typing indicator request completed
    | ReceiveStoreData Decode.Value -- We are send back a value on request from localStorage.
    | TypingTick Posix -- Tick for updating the typing status
    | History RoomId -- Load history for a room
    | ReceiveHistoryResponse RoomId (Result Http.Error HistoryResponse) -- HTTP, receive history
    | SendImages RoomId -- Image selection triggered
    | SendFiles RoomId -- File selection triggered
    | ImagesSelected RoomId File (List File) -- Images to send selected
    | FilesSelected RoomId File (List File) -- Files to send selected
    | ImageUploadComplete RoomId File (Result Http.Error String) -- Image has been uploaded
    | FileUploadComplete RoomId File (Result Http.Error String) -- File has been uploaded
    | SendImageResponse (Result Http.Error String) -- Server responded to image
    | SendFileResponse (Result Http.Error String) -- Server responded to file
    | ReceiveMarkdown MarkdownResponse -- Markdown was rendered
    | DismissError Int -- User dismisses error
    | AttemptReconnect -- User wants to reconnect to server
    | UpdateSearchText String -- Change search text in room list

displayName : Dict String UserData -> Username -> String
displayName ud s = Maybe.withDefault (senderName s) <| Maybe.andThen .displayName <| Dict.get s ud

roomDisplayName : Dict RoomId String -> RoomId -> String
roomDisplayName rd rid =
    Maybe.withDefault "<No Name>" <| Dict.get rid rd

computeRoomDisplayName : Dict String UserData -> Maybe AccountData -> RoomId -> JoinedRoom -> Maybe String
computeRoomDisplayName ud ad rid jr =
    let
        customName = roomName jr
        direct = ad
            |> Maybe.andThen .events
            |> Maybe.andThen (findFirst ((==) "m.direct" << .type_))
            |> Maybe.map (Decode.decodeValue directMessagesDecoder << .content)
            |> Maybe.andThen Result.toMaybe
            |> Maybe.andThen (Dict.get rid)
    in
        case (customName, direct) of
            (Just s, _) -> customName
            (_, Just u) -> direct
            _ -> Nothing

computeRoomsDisplayNames : Dict String UserData -> SyncResponse -> Dict String String
computeRoomsDisplayNames ud sr =
    sr.rooms
    |> Maybe.andThen .join
    |> Maybe.map Dict.toList
    |> Maybe.map (List.foldl
        (\(rid, jr) d ->
            computeRoomDisplayName ud sr.accountData rid jr 
            |> Maybe.map (\n -> Dict.insert rid n d) 
            |> Maybe.withDefault d) Dict.empty)
    |> Maybe.withDefault Dict.empty

roomUrl : String -> String
roomUrl s = Url.Builder.absolute [ "room", s ] []

loginUrl : String
loginUrl = Url.Builder.absolute [ "login" ] []

newUsers : Model -> List Username -> List Username
newUsers m lus = List.filter (\u -> not <| Dict.member u m.userData) lus

joinedRooms : Model -> Dict RoomId JoinedRoom
joinedRooms m = Maybe.withDefault Dict.empty <| Maybe.andThen .join <| m.sync.rooms

currentRoom : Model -> Maybe JoinedRoom
currentRoom m =
    Maybe.andThen (\s -> Dict.get s <| joinedRooms m) <| currentRoomId m

currentRoomId : Model -> Maybe RoomId
currentRoomId m = case m.route of
    Room r -> Just r
    _ -> Nothing
