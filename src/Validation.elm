module Validation exposing (validate, validateGroup, validatePersonInGroup)

import Data.Person as Person exposing (Person)
import Data.Group as Group exposing (Group)
import Dict
import Set exposing (Set)


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


validateGroup : Person.Dict -> Group -> List String
validateGroup people group =
    let
        members =
            group.people
                |> Set.toList
                |> List.filterMap (flip Dict.get people)

        leaders =
            List.filter ((==) Person.Asgl << .role) members

        validators =
            [ validateEnoughLeaders leaders
            , validateLeaderGenders members leaders
            -- , validateLeaderFreeTimes leaders
            ]
    in
        List.concat validators

validatePersonInGroup : Group -> Person -> List String
validatePersonInGroup group =
    validate (List.member group.time << .free) (flip (++) " is not free for this group" << .name)
