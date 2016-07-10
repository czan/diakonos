module Util exposing (..)

(!!) : (a, b) -> c -> (a, b, c)
(!!) (a, b) c = (a, b, c)

takeWhile : (a -> Bool) -> List a -> List a
takeWhile f list =
    case list of
        [] -> []
        (x :: xs) ->
            if f x then
                x :: takeWhile f xs
            else
                []

dropWhile : (a -> Bool) -> List a -> List a
dropWhile f list =
    case list of
        [] -> []
        (x :: xs) ->
            if f x then
                dropWhile f xs
            else
                xs
