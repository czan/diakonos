module Data.Person exposing (Person, Id, Dict, Role(..), Gender(..), roles, genders, decode, encode, drag, idDrag)

import Data.Timeslot as Timeslot exposing (Timeslot)
import Json.Decode as Json exposing ((:=))
import Json.Encode as E
import String
import Dict
import DragAndDrop

type alias Id = String

type alias Person =
    { name : String
    , role : Role
    , gender : Gender
    , free : List Timeslot }

type alias Dict = Dict.Dict Id Person

type Role = Howie
          | Asgl
          | Member

roles : List Role
roles = [Howie, Asgl, Member]

type Gender = Male
            | Female

genders : List Gender
genders = [Male, Female]

    
decodeRole : Json.Decoder Role
decodeRole =
    Json.string `Json.andThen`
        (\x -> case String.toLower x of
                   "asgl" -> Json.succeed Asgl
                   "member" -> Json.succeed Member
                   "howie" -> Json.succeed Howie
                   _ -> Json.fail "Invalid role")

decodeGender : Json.Decoder Gender
decodeGender =
    Json.string `Json.andThen`
        (\x -> case String.toLower x of
                   "male" -> Json.succeed Male
                   "female" -> Json.succeed Female
                   _ -> Json.fail "Invalid gender")

decode : Json.Decoder Person
decode =
    Json.object4 Person
        ("name" := Json.string)
        ("role" := decodeRole)
        ("gender" := decodeGender)
        ("free" := Json.list Timeslot.decode)

encode : Person -> E.Value
encode {name, role, gender, free} =
    E.object
        [ ("name", E.string name)
        , ("role", E.string <| toString role)
        , ("gender", E.string <| toString gender)
        , ("free", E.list <| List.map Timeslot.encode free)
        ]
        
drag : DragAndDrop.DragType Person
drag =
    { key = "Person"
    , encode = encode
    , decode = decode
    }

idDrag : DragAndDrop.DragType Id
idDrag =
    { key = "PersonId"
    , encode = E.string
    , decode = Json.string
    }
