module Operations exposing ( countValues, countIncomparableValues
                           , combineIncomparableValues, unique
                           )

import Dict exposing (Dict)

-- This file contains a lot of helper functions.

unique : List a -> List a
unique la =
    let
        trackUnique : List a -> List a -> List a
        trackUnique toSee soFar =
            case toSee of
                [] ->
                    soFar
                
                head :: tail ->
                    if List.member head soFar then
                        trackUnique tail soFar
                    else
                        trackUnique tail (soFar ++ [head])
    in
        trackUnique la []

countValues : List comparable -> Dict comparable Int
countValues la =
    trackValues la Dict.empty


trackValues : List comparable -> Dict comparable Int -> Dict comparable Int
trackValues la d =
    case la of
        [] ->
            d
        
        head :: tail ->
            let
                count =
                    case Dict.get head d of
                        Just v ->
                            v + 1
                        Nothing ->
                            1
            in
                Dict.insert head count d
                    |> trackValues tail


-- This function counts values without a dictionary.
-- It is less efficient than `countValues` but it
-- works on records.
countIncomparableValues : List a -> List (a, Int)
countIncomparableValues la =
    let
        addValue : a -> List (a, Int) -> List (a, Int)
        addValue x lx =
            if List.member x (List.map (\(v, _) -> v) lx) then
                List.map (updateValue x) lx
            else
                lx ++ [(x, 1)]

        updateValue : a -> (a, Int) -> (a, Int)
        updateValue v (x, i) =
            if x == v then
                (x, i+1)
            else
                (x, i)
    in
        List.foldr addValue [] la

combineIncomparableValues : List (a, Int) -> List (a, Int)
combineIncomparableValues la =
    let
        count : a -> Int
        count item =
            la
                |> List.filter (\(t, _) -> t == item)
                |> List.map    (\(_, i) -> i)
                |> List.sum
    in
        la
            |> List.map (\(t, _) -> t)
            |> unique
            |> List.map (\t -> (t, count t))
