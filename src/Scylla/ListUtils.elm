module Scylla.ListUtils exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)

groupBy : (a -> comparable) -> List a -> Dict comparable (List a)
groupBy f xs =
    let
        update v ml = case ml of
            Just l -> Just (v::l)
            Nothing -> Just [ v ]
    in
        List.foldl (\v acc -> Dict.update (f v) (update v) acc) Dict.empty xs

uniqueByTailRecursive : (a -> comparable) -> List a -> Set comparable -> List a -> List a
uniqueByTailRecursive f l s acc =
    case l of
        x::tail ->
            if Set.member (f x) s
            then uniqueByTailRecursive f tail s acc
            else uniqueByTailRecursive f tail (Set.insert (f x) s) (x::acc)
        [] -> List.reverse acc

uniqueBy : (a -> comparable) -> List a -> List a
uniqueBy f l = uniqueByTailRecursive f l Set.empty []

findFirst : (a -> Bool) -> List a -> Maybe a
findFirst cond l = case l of
    x::xs -> if cond x then Just x else findFirst cond xs
    [] -> Nothing

findLast : (a -> Bool) -> List a -> Maybe a
findLast cond l = findFirst cond <| List.reverse l

findFirstBy : (a -> comparable) -> (a -> Bool) -> List a -> Maybe a
findFirstBy sortFunction cond l = findFirst cond <| List.sortBy sortFunction l

findLastBy : (a -> comparable) -> (a -> Bool) -> List a -> Maybe a
findLastBy sortFunction cond l = findLast cond <| List.sortBy sortFunction l

