module Operations exposing (countValues, countIncomparableValues)
import Dict exposing (Dict)
import Html exposing (a)

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
    trackIncomparableValues la [] []

trackIncomparableValues : List a -> List a -> List Int -> List (a, Int)
trackIncomparableValues la seenSoFar counts =
    case la of
        [] ->
            List.map2 (\s c -> (s, c)) seenSoFar counts
        
        head :: tail ->
            if List.member head seenSoFar then
                trackIncomparableValues tail seenSoFar counts
            else
                let
                    occurrences : Int
                    occurrences = countIncomparableValue head la
                in
                    trackIncomparableValues
                        tail
                        (seenSoFar ++ [head])
                        (counts ++ [occurrences])

-- NOTICE: Without an s
countIncomparableValue : a -> List a -> Int
countIncomparableValue a la =
    case la of
        [] ->
            0
        
        head :: tail ->
            if head == a then
                countIncomparableValue a tail + 1
            else
                countIncomparableValue a tail