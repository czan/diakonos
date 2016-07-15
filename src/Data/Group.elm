module Data.Group exposing (Group, Id, Dict, Index, decode, encode, idDrag, makeIndex)

import Data.Timeslot as Timeslot exposing (Timeslot)
import Data.Person as Person
import Set exposing (Set)
import Json.Decode as Json exposing ((:=))
import Json.Encode as E
import Dict
import DragAndDrop as DragAndDrop

type alias Id = String


type alias Group =
    { time : Timeslot
    , people : Set Person.Id
    }


type alias Dict = Dict.Dict Id Group


type alias Index =
    { byTimeslot : Timeslot -> Maybe (Id, Group)
    , byPerson : Person.Id -> Maybe (Id, Group)
    }


decode : Json.Decoder Group
decode =
    Json.object2 Group
        ("time" := Timeslot.decode)
        ("people" := (Json.map Set.fromList <| Json.list Json.string))


encode : Group -> E.Value
encode {time, people} =
    E.object
        [ ("time", Timeslot.encode time)
        , ("people", people
               |> Set.toList
               |> List.map E.string
               |> E.list
          )
        ]


idDrag : DragAndDrop.DragType Id
idDrag =
    { key = "GroupId"
    , encode = E.string
    , decode = Json.string
    }



makeIndex : Dict -> Index
makeIndex groups =
    { byTimeslot = timeIndexedGroups groups
    , byPerson = personIndexedGroups groups
    }


timeIndexedGroups : Dict -> Timeslot -> Maybe (Id, Group)
timeIndexedGroups groups =
    let
        indexGroup id group dict =
            Dict.insert (toString group.time) (id, group) dict

        index = Dict.foldl indexGroup Dict.empty groups
    in
        \timeslot -> Dict.get (toString timeslot) index


personIndexedGroups : Dict -> Person.Id -> Maybe (Id, Group)
personIndexedGroups groups =
    let
        indexGroup id group dict =
            Set.foldl (flip Dict.insert (id, group)) dict group.people

        index = Dict.foldl indexGroup Dict.empty groups
    in
        \id -> Dict.get id index
