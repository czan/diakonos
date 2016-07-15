module Util exposing (..)

import Char
import String
import Random


(?) : Maybe a -> a -> a
(?) = flip Maybe.withDefault


(!!) : (a, b) -> c -> (a, b, c)
(!!) (a, b) c = (a, b, c)


isJust : Maybe a -> Bool
isJust value =
    case value of
        Just _ ->
            True
        _ ->
            False


takeWhile : (a -> Bool) -> List a -> List a
takeWhile f list =
    case list of
        [] ->
            []

        (x :: xs) ->
            if f x then
                x :: takeWhile f xs
            else
                []


dropWhile : (a -> Bool) -> List a -> List a
dropWhile f list =
    case list of
        [] ->
            []

        (x :: xs) ->
            if f x then
                dropWhile f xs
            else
                xs


hexChar : Int -> Char
hexChar input =
    let
        code =
            if input < 10 then
                48 + input
            else
                97 + (input - 10)
    in
        Char.fromCode code


hex : Int -> String
hex number =
    if number < 0 then
        "-" ++ hex (-number)
    else
        if number < 16 then
            hexChar number |> String.fromChar
        else
            hex (number // 16) ++ hex (number % 16)


hexString : List Int -> String
hexString list =
    list
        |> List.map (String.fromChar << hexChar)
        |> String.join ""

           
idGenerator : Random.Generator String
idGenerator =
    Random.int 0 15
        |> Random.list 20
        |> Random.map hexString
