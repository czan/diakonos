module Validation exposing (validate, validateGroup, validatePersonInGroup)

import Data.Person as Person exposing (Person)
import Data.Group as Group exposing (Group)
import Data.Timeslot as Timeslot exposing (Timeslot)
import Dict
import Set exposing (Set)
import Util exposing (intersect)


validate : (a -> Bool) -> (a -> String) -> a -> List String
validate pred message value =
    if pred value then
        []
    else
        [ message value ]


hasGender : Person.Gender -> Person -> Bool
hasGender gender = ((==) gender << .gender)


validateLeaderGenders : List Person -> List Person -> List String
validateLeaderGenders members leaders =
    let
        validateMale = if List.any (hasGender Person.Male) members then
                           [ validate (List.any (hasGender Person.Male))
                                 (always "Must have at least one male leader") ]
                       else
                           []
        validateFemale = if List.any (hasGender Person.Female) members then
                           [ validate (List.any (hasGender Person.Female))
                                 (always "Must have at least one female leader") ]
                       else
                           []
        run validate = validate leaders
    in List.concatMap run (validateMale ++ validateFemale)


validateEnoughLeaders : List Person -> List String
validateEnoughLeaders leaders =
    validate (\x -> List.length x > 1) (always "Not enough leaders (minimum 2)") leaders


validateLeaderFreeTimes : Timeslot -> List Person -> List String
validateLeaderFreeTimes timeslot leaders =
    if List.length leaders < 2 then
        []
    else
        let
            removeTimeslot =
                List.filter ((/=) timeslot)
            haveCommonFreeTime leaders =
                leaders
                    |> List.map (removeTimeslot << .free)
                    |> intersect
                    |> List.isEmpty
                    |> not
        in
            validate haveCommonFreeTime (always "Leaders do not have another common free time") leaders


validateGroup : Person.Dict -> Group -> List String
validateGroup people group =
    let
        members =
            group.people
                |> Set.toList
                |> List.filterMap (flip Dict.get people)

        leaders =
            List.filter ((==) Person.Leader << .role) members

        validators =
            [ validateEnoughLeaders leaders
            , validateLeaderGenders members leaders
            , validateLeaderFreeTimes group.time leaders
            ]
    in
        List.concat validators

validatePersonInGroup : Group -> Person -> List String
validatePersonInGroup group =
    validate (List.member group.time << .free) (flip (++) " is not free for this group" << .name)
