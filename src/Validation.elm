module Validation exposing (validate, groupErrors, validateGroup)

import Data.Person as Person exposing (Person)
import Data.Group as Group exposing (Group)
import Dict
import Set exposing (Set)


groupErrors : List (Result (List String) a) -> Result (List String) (List a)
groupErrors results =
    let grouping acc result =
            case (acc, result) of
                (Ok value, Ok value') ->
                    Ok (value :: value')
                (Ok value, Err errs) ->
                    Err errs
                (Err errs, Ok value') ->
                    Err errs
                (Err errs, Err errs') ->
                    Err (errs ++ errs')
    in List.foldl grouping (Ok []) results


validate : (a -> Bool) -> (a -> String) -> a -> Result (List String) a
validate pred message value =
    if pred value then
        Ok value
    else
        Err [ message value ]


validateEnoughLeaders : List Person -> Result (List String) ()
validateEnoughLeaders leaders =
    let male = List.filter ((==) Person.Male << .gender) leaders
        female = List.filter ((==) Person.Female << .gender) leaders
    in
        groupErrors [ validate (\x -> List.length x > 1) (always "Not enough leaders (minimum 2)") leaders
                    , validate (not << List.isEmpty) (always "No male leaders") male
                    , validate (not << List.isEmpty) (always "No female leaders") female
                    ]
            |> Result.map (always ())


validateGroup : Person.Dict -> Group -> Result (List String) Group
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
            -- , validateLeaderFreeTimes leaders
            ]
    in
        groupErrors validators
            |> Result.map (always group)
