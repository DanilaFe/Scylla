module Scylla.Route exposing (..)
import Url.Parser exposing (Parser, oneOf, map, s, string, (</>), top)

type alias RoomId = String

type Route =
    Base
    | Unknown
    | Login
    | Room RoomId

route : Parser (Route -> a) a
route =
    oneOf
        [ map Base top
        , map Login (s "login")
        , map Room (s "room" </> string)
        ]
