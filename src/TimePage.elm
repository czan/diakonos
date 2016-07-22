module TimePage exposing (Model, Msg, ParentMsg(..), init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onMouseOver, onMouseOut, onClick)
import Dict exposing (Dict)
import Set exposing (Set)
import Random
import Validation exposing (validateGroup, validatePersonInGroup)
import Data.Group as Group exposing (Group)
import Data.Person as Person exposing (Person)
import Data.Timeslot as Timeslot exposing (Timeslot, Day(..))
import Util exposing ((!!), hex, idGenerator, isJust)
import DragAndDrop exposing (draggable, dragData, dropTarget, onDrop)
import String


type HoverState
    = HoverPerson Person.Id
    | HoverTimeslot Timeslot
    | NoHover

type alias Model =
    { groups : Group.Dict
    , people : Person.Dict
    , hover : HoverState
    }


type Msg
    = Add Timeslot (Maybe Person.Id)
    | Create Group.Id Timeslot (Maybe Person.Id)
    | Remove Person.Id
    | Delete Timeslot
    | Move Group.Id Timeslot
    | Hover HoverState


type ParentMsg
    = SaveGroups Group.Dict
    | None


init : Person.Dict -> Group.Dict -> (Model, Cmd Msg)
init people groups =
    { groups = groups
    , people = people
    , hover = NoHover
    } ! []


fixGroup : Timeslot -> Person.Id -> Group.Id -> Group -> Group
fixGroup time personId groupId group =
    let people' = if time == group.time then
                      Set.insert personId group.people
                  else
                      Set.remove personId group.people
    in { group | people = people' }

        
update : Msg -> Model -> (Model, Cmd Msg, ParentMsg)
update msg model =
    let index = Group.makeIndex model.groups
        create personId timeslot id = Create id timeslot personId
    in case msg of
           Add timeslot personId ->
               case index.byTimeslot timeslot of
                   Nothing ->
                       model ! [ Random.generate (create personId timeslot) idGenerator ] !! None

                   _ ->
                       case personId of
                           Nothing ->
                               model ! [] !! SaveGroups model.groups

                           Just id ->
                               let groups' = model.groups
                                           |> Dict.map (fixGroup timeslot id)
                               in { model | groups = groups' } ! [] !! SaveGroups groups'

           Create id timeslot personId ->
               let group = { time = timeslot, people = Set.empty }
                   groups' = Dict.insert id group model.groups
               in update (Add timeslot personId) { model | groups = groups' }

           Move id timeslot ->
               case (Dict.get id model.groups, index.byTimeslot timeslot) of
                   (Just group, Nothing) ->
                       let group' = { group | time = timeslot }
                           groups' = Dict.insert id group model.groups
                       in { model | groups = groups' } ! [] !! SaveGroups groups'

                   _ ->
                       model ! [] !! None

           Remove personId ->
               case index.byPerson personId of
                   Just (id, group) ->
                       let people' = Set.remove personId group.people
                           group' = { group | people = people' }
                           groups' = Dict.insert id group' model.groups
                       in
                           { model | groups = groups' } ! [] !! SaveGroups groups'

                   Nothing ->
                       model ! [] !! None

           Delete timeslot ->
               case index.byTimeslot timeslot of
                   Just (id, group) ->
                       if Set.isEmpty group.people then
                           let groups' = Dict.remove id model.groups
                           in { model | groups = groups' } ! [] !! SaveGroups groups'
                       else
                           model ! [] !! None

                   Nothing ->
                       model ! [] !! None

           Hover state ->
               { model | hover = state } ! [] !! None


hasRole : Person.Role -> Person.Id -> Person -> Bool
hasRole role id person = person.role == role


splitRoles : Person.Dict -> {leaders : Person.Dict, members : Person.Dict}
splitRoles people =
    { leaders = Dict.filter (hasRole Person.Leader) people
    , members = Dict.filter (hasRole Person.Member) people
    }


viewGroupPerson : HoverState -> Group -> (Person.Id, Person) -> Html Msg
viewGroupPerson hover group (id, person) =
    let highlighted = case hover of
                          HoverTimeslot timeslot ->
                              person.free
                                  |> List.any ((==) timeslot)
                          _ ->
                              False
        errors = validatePersonInGroup group person
    in div [ A.classList [ ("person", True)
                         , ("leader", person.role == Person.Leader)
                         , ("highlight", highlighted)
                         , ("error", not <| List.isEmpty errors)
                         ]
           , onMouseOver (Hover <| HoverPerson id)
           , onMouseOut (Hover <| NoHover)
           ] [ span (List.concat [ [ draggable True
                                   , dragData Person.idDrag id
                                   ]
                                 , if List.isEmpty errors then
                                       []
                                   else
                                       [ A.title (String.join "\n" errors) ]
                                 ])
                   [ text person.name ] ]



viewPerson : HoverState -> Group.Index -> (Person.Id, Person) -> Html Msg
viewPerson hover index (id, person) =
    let noMatchingGroups = person.free
                         |> List.map index.byTimeslot
                         |> List.all ((==) Nothing)
        highlighted = case hover of
                          HoverTimeslot timeslot ->
                              person.free
                                  |> List.any ((==) timeslot)
                          _ ->
                              False
    in div ([ A.classList [ ("person", True)
                          , ("in-group", index.byPerson id /= Nothing)
                          , ("no-group", noMatchingGroups)
                          , ("leader", person.role == Person.Leader)
                          , ("highlight", highlighted)
                          ]
            , onMouseOver (Hover <| HoverPerson id)
            , onMouseOut (Hover <| NoHover)
            ] ++ if noMatchingGroups then
                     [ A.title (person.name ++ " is not free for any groups") ]
                 else
                     [])
        [ span [ draggable True
               , dragData Person.idDrag id
               ]
              [ text person.name ] ]


viewPeople : HoverState -> Group.Index -> String -> Dict Person.Id Person -> Html Msg
viewPeople hover index title people =
    div [ A.class "person-list"
        , dropTarget True
        , onDrop Person.idDrag Remove
        ] (h1 [] [ text title ] ::
               (Dict.toList people
               |> List.sortBy (.name << snd)
               |> List.sortBy (List.length << .free << snd)
               |> List.map (viewPerson hover index)))


view : Model -> Html Msg
view model =
    let {people, groups} = model
        {leaders, members} = splitRoles people
        weights = peopleWeights <| Dict.values people
        index = Group.makeIndex model.groups
        fns = { weight = weights
              , leaders = peoplePerTimeslot <| Dict.values leaders
              , members = peoplePerTimeslot <| Dict.values members
              , index = index
              }
    in div [ A.class "time-page" ]
        [ div [ A.class "timetable" ]
              [ Timeslot.table [] <| viewTimeslot model.hover people fns ]
        , viewPeople model.hover index "Leaders" leaders
        , viewPeople model.hover index "Members" members
        ]


addPersonWeights : Person -> Dict String Float -> Dict String Float
addPersonWeights person weights =
    let length = List.length person.free
        value = 1 / (toFloat length)
        addValue v = Just (value + Maybe.withDefault 0 v)
        insertValue timeslot = Dict.update (toString timeslot) addValue
    in List.foldl insertValue weights person.free


normalizeWeights : Dict String Float -> Dict String Float
normalizeWeights weights =
    let maxValue = Dict.foldl (always Basics.max) 0 weights
        normalize x = x / maxValue
    in Dict.map (always normalize) weights


peopleWeights : List Person -> Timeslot -> Float
peopleWeights people =
    let values = List.foldl addPersonWeights Dict.empty people
                 |> normalizeWeights
    in \timeslot -> Dict.get (toString timeslot) values |> Maybe.withDefault 0


peoplePerTimeslot : List Person -> Timeslot -> List Person
peoplePerTimeslot people =
    let addValue person = (Maybe.withDefault []) >> ((::) person) >> Just
        insertTimeslot person timeslot = Dict.update (toString timeslot) (addValue person)
        addPerson person dict = List.foldl (insertTimeslot person) dict person.free
        values = List.foldl addPerson Dict.empty people
    in \timeslot -> Dict.get (toString timeslot) values |> Maybe.withDefault []


valueToBackgroundColor : Float -> String
valueToBackgroundColor value =
    let weight = value * 255 |> truncate
        colorPart = (if weight < 16 then "0" else "") ++ hex weight
    in "#" ++ colorPart ++ colorPart ++ colorPart


valueToTextColor : Float -> String
valueToTextColor value =
    if value < 0.7 then "#fff" else "#000"


type alias TimeslotFunctions =
    { leaders : Timeslot -> List Person
    , members : Timeslot -> List Person
    , weight : Timeslot -> Float
    , index : Group.Index
    }


freeAt : Person -> Timeslot -> Bool
freeAt person timeslot =
    List.member timeslot person.free


sortSecond : (b -> b -> Order) -> (a, b) -> (a, b) -> Order
sortSecond compare left right =
    compare (snd left) (snd right)


leadersFirst : Person -> Person -> Order
leadersFirst left right =
    case (left.role, right.role) of
        (Person.Leader, Person.Member) ->
            LT

        (Person.Member, Person.Leader) ->
            GT

        (_, _) ->
            compare left.name right.name


viewTimeslot : HoverState -> Person.Dict -> TimeslotFunctions -> Timeslot -> Html Msg
viewTimeslot hover people {leaders, members, weight, index} timeslot =
    let
        leaderCount =
            leaders timeslot |> List.length
        memberCount =
            members timeslot |> List.length
        background =
            weight timeslot |> valueToBackgroundColor
        foreground =
            weight timeslot |> valueToTextColor
        lookupPerson id =
            case Dict.get id people of
                Just person ->
                    [(id, person)]
                Nothing ->
                    []

        group = index.byTimeslot timeslot

        highlighted = case hover of
                          HoverPerson id ->
                              Dict.get id people
                                  |> Maybe.map (flip freeAt timeslot)
                                  |> Maybe.withDefault False

                          _ ->
                              False

        errors =
            index.byTimeslot timeslot
                |> Maybe.map (validateGroup people << snd)
                |> Maybe.withDefault []
    in
        td [ A.style [ ("background-color", background)
                     , ("color", foreground)
                     ]
           , A.classList [ ("has-group", isJust group)
                         , ("highlight", highlighted)
                         , ("error", not <| List.isEmpty errors)
                         ]
           , A.title (String.join "\n" errors)
           , dropTarget True
           , onDrop Person.idDrag (Add timeslot << Just)
           -- , onDrop Group.idDrag (flip Move timeslot)
           , onMouseOver (Hover <| HoverTimeslot timeslot)
           , onMouseOut (Hover <| NoHover)
           , onClick (if isJust group then Delete timeslot else Add timeslot Nothing)
           ] (case group of
                  Just (id, g) ->
                      if Set.isEmpty g.people then
                          [ text (toString leaderCount ++ " / " ++ toString memberCount)
                          ]
                      else
                          (g.people
                          |> Set.toList
                          |> List.concatMap lookupPerson
                          |> List.sortWith (sortSecond leadersFirst)
                          |> List.map (viewGroupPerson hover g))
                  Nothing ->
                      [ text (toString leaderCount ++ " / " ++ toString memberCount)
                      ])


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
