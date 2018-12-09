module Scylla.Views exposing (..)
import Scylla.Model exposing (..)
import Scylla.Sync exposing (..)
import Json.Decode as Decode
import Html exposing (Html, div, input, text, button, div, span)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput, onClick)
import Dict

viewFull : Model -> List (Html Msg)
viewFull model = 
    let
        core = case model.token of
            Just _ -> normalView model
            Nothing -> loginView model 
        errorList = errorsView model.errors
    in
        [ errorList ] ++ [ core ] 

errorsView : List String -> Html Msg
errorsView = div [] << List.map errorView

errorView : String -> Html Msg
errorView s = div [] [ text s ]

normalView : Model -> Html Msg
normalView m = div [] []

loginView : Model -> Html Msg
loginView m = div []
    [ input [ type_ "text", value m.loginUsername, onInput ChangeLoginUsername] []
    , input [ type_ "password", value m.loginPassword, onInput ChangeLoginPassword ] []
    , input [ type_ "text", value m.apiUrl, onInput ChangeApiUrl ] []
    , button [ onClick AttemptLogin ] [ text "Log In" ]
    ]

joinedRoomView : Model -> JoinedRoom -> Html Msg
joinedRoomView m jr =
    let
        events = Maybe.withDefault [] <| Maybe.andThen .events jr.timeline
        renderedEvents = List.filterMap (eventView m) events
        eventContainer = eventContainerView m renderedEvents
    in
        div [] [ eventContainer ]

eventContainerView : Model -> List (Html Msg) -> Html Msg
eventContainerView m = div []

eventView : Model -> RoomEvent -> Maybe (Html Msg)
eventView m re = case re.type_ of
    "m.room.message" -> messageView m re
    _ -> Nothing

messageView : Model -> RoomEvent -> Maybe (Html Msg)
messageView m re =
    let
        msgtype = Decode.decodeValue (Decode.field "msgtype" Decode.string) re.content
    in
        case msgtype of
            Ok "m.text" -> messageTextView m re
            _ -> Nothing

messageTextView : Model -> RoomEvent -> Maybe (Html Msg)
messageTextView m re =
    let
        body = Decode.decodeValue (Decode.field "body" Decode.string) re.content
        wrap mtext = div []
            [ span [] [ text re.sender ]
            , span [] [ text mtext ]
            ]
    in
        Maybe.map wrap <| Result.toMaybe body
