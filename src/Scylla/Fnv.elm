module Scylla.Fnv exposing (..)
import Bitwise

hash : String -> Int
hash = String.foldl hashChar 2166136261

hashChar : Char -> Int -> Int
hashChar char h = modBy 4294967295
    <| (Bitwise.xor h <| Char.toCode char) * 16777619
