module Main exposing (main)

import Html.App as App
import Data.Person as Person
import Data.Group as Group
import Json.Decode as D exposing ((:=))
import Dict
import Pages


main : Program { name : D.Value, people : D.Value, groups : D.Value }
main =
    let init {name, people, groups} =
            let name' = Result.withDefault "" <| D.decodeValue D.string name
                people' = Result.withDefault Dict.empty <| D.decodeValue (D.dict Person.decode) people
                groups' = Result.withDefault Dict.empty <| D.decodeValue (D.dict Group.decode) groups
            in Pages.init name' people' groups'
    in App.programWithFlags
        { init = init
        , view = Pages.view
        , update = Pages.update
        , subscriptions = Pages.subscriptions
        }
