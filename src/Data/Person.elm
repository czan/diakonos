module Data.Person exposing (Person, Id, Dict, Role(..), Gender(..), roles, genders, jsonDecode)

import Data.Timeslot as Timeslot exposing (Timeslot)
import Json.Decode as Json exposing ((:=))
import String
import Dict

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

jsonDecode : Json.Decoder Person
jsonDecode =
    Json.object4 Person
        ("name" := Json.string)
        ("role" := decodeRole)
        ("gender" := decodeGender)
        ("free" := (Json.list Timeslot.jsonDecode))
