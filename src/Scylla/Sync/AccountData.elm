module Scylla.Sync.AccountData exposing (..)
import Scylla.ListUtils exposing (..)
import Scylla.Sync.DecodeTools exposing (maybeDecode)
import Scylla.Sync.Events exposing (Event, eventDecoder)
import Json.Decode as Decode exposing (Decoder, list, decodeValue)
import Dict exposing (Dict)

type alias AccountData =
    { events : Maybe (List Event)
    }

accountDataDecoder : Decoder AccountData
accountDataDecoder =
    Decode.succeed AccountData
        |> maybeDecode "events" (list eventDecoder)

type alias DirectMessages = Dict String String

directMessagesDecoder : Decode.Decoder DirectMessages
directMessagesDecoder =
    Decode.dict (Decode.list Decode.string)
        |> Decode.map (invertDirectMessages)

type alias DirectMessagesRaw = Dict String (List String)

invertDirectMessages : DirectMessagesRaw -> DirectMessages
invertDirectMessages dmr =
    Dict.foldl
        (\k lv acc -> List.foldl (\v -> Dict.insert v k) acc lv)
        Dict.empty
        dmr

applyAccountData : Maybe AccountData -> AccountData -> AccountData
applyAccountData mad ad =
    case mad of
        Nothing -> ad
        Just newAd ->
            case (newAd.events, ad.events) of
                (Just es, Nothing) -> newAd
                (Just newEs, Just es) -> { events = Just (newEs ++ es) }
                _ -> ad

getAccountData : String -> Decode.Decoder a -> AccountData -> Maybe a
getAccountData key d ad = ad.events
    |> Maybe.andThen (findFirst ((==) key << .type_))
    |> Maybe.map .content
    |> Maybe.andThen (Result.toMaybe << decodeValue d)

getDirectMessages : AccountData -> Maybe DirectMessages
getDirectMessages = getAccountData "m.direct" directMessagesDecoder
