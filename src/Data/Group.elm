module Data.Group exposing (Group, Id, Dict, jsonDecode)

import Data.Timeslot as Timeslot exposing (Timeslot)
import Data.Person as Person
import Set exposing (Set)
import Json.Decode as Json exposing ((:=))
import Dict

type alias Id = String

type alias Group =
    { time : Timeslot
    , people : Set Person.Id
    }

type alias Dict = Dict.Dict Id Group

jsonDecode : Json.Decoder Group
jsonDecode =
    Json.object2 Group
        ("time" := Timeslot.jsonDecode)
        ("people" := Json.map Set.fromList (Json.list Json.string))
