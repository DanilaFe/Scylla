module Scylla.Views exposing (..)
import Scylla.Model exposing (..)
import Html exposing (Html, div, input, text, button)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput, onClick)

viewFull : Model -> Html Msg
viewFull model = case model.token of
    Just _ -> normalView model
    Nothing -> loginView model 

normalView : Model -> Html Msg
normalView m = div [] [ text "You are logged in!" ]

loginView : Model -> Html Msg
loginView m = div []
    [ input [ type_ "text", value m.loginUsername, onInput ChangeLoginUsername] []
    , input [ type_ "password", value m.loginPassword, onInput ChangeLoginPassword ] []
    , input [ type_ "text", value m.apiUrl, onInput ChangeApiUrl ] []
    , button [ onClick AttemptLogin ] [ text "Log In" ]
    ]
