port module Scylla.Markdown exposing (..)

type alias MarkdownRequest =
    { roomId : String
    , text : String
    }

type alias MarkdownResponse =
    { roomId : String
    , text : String
    , markdown : String
    }

port requestMarkdownPort : MarkdownRequest -> Cmd msg
port receiveMarkdownPort : (MarkdownResponse -> msg) -> Sub msg
