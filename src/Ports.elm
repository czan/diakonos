port module Ports exposing (exportData, importData, importedData, saveData, scrollToVisible, runSolver, solverSolution)

import Json.Encode as E exposing (Value)
import Data.Person as Person
import Data.Group as Group
import Dict

port scrollToVisible : String -> Cmd a

port exportDataPort : { name: String, data: Value } -> Cmd a

exportData : String -> (Person.Dict, Group.Dict) -> Cmd a
exportData name (people, groups) =
    let people' = E.object <| Dict.toList <| Dict.map (always Person.encode) people
        groups' = E.object <| Dict.toList <| Dict.map (always Group.encode) groups
    in exportDataPort { name = name
                      , data = E.object [ ("name", E.string name)
                                        , ("people", people')
                                        , ("groups", groups')
                                        ]
                      }

port importDataPort : () -> Cmd a

importData : () -> Cmd a
importData = importDataPort

port importedDataPort : (Value -> msg) -> Sub msg

importedData : (Value -> msg) -> Sub msg
importedData = importedDataPort

port saveDataPort : { name : String, people : Value, groups : Value } -> Cmd a

saveData : String -> (Person.Dict, Group.Dict) -> Cmd a
saveData name (people, groups) =
    let people' = E.object <| Dict.toList <| Dict.map (always Person.encode) people
        groups' = E.object <| Dict.toList <| Dict.map (always Group.encode) groups
    in saveDataPort { name = name
                    , people = people'
                    , groups = groups'
                    }

port runSolverPort : { people : Value, numGroups : Int, mixed : Bool, leftOut : Int } -> Cmd a

runSolver : Person.Dict -> Int -> Bool -> Int -> Cmd a
runSolver people num mixed leftOut =
    let people' = E.object <| Dict.toList <| Dict.map (always Person.encode) people
    in runSolverPort { people = people'
                     , numGroups = num
                     , mixed = mixed
                     , leftOut = leftOut
                     }

port solverSolutionPort : (Value -> msg) -> Sub msg

solverSolution : (Value -> msg) -> Sub msg
solverSolution = solverSolutionPort
