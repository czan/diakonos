module TimePage exposing (Model, Msg, ParentMsg(..), init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onMouseOver, onMouseOut, onClick)
import Dict exposing (Dict)
import Set exposing (Set)
import Random
import Data.Group as Group exposing (Group)
import Data.Person as Person exposing (Person)
import Data.Timeslot as Timeslot exposing (Timeslot, Day(..))
import Util exposing ((!!), hex, idGenerator)
import DragAndDrop exposing (draggable, dragData, dropTarget, onDrop)


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
    = Add Timeslot Person.Id
    | Create Group.Id Timeslot Person.Id
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
               if index.byTimeslot timeslot == Nothing then
                   model ! [ Random.generate (create personId timeslot) idGenerator ] !! None
               else
                   let groups' = model.groups
                               |> Dict.map (fixGroup timeslot personId)
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
    { leaders = Dict.filter (hasRole Person.Asgl) people
    , members = Dict.filter (hasRole Person.Member) people
    }


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
    in div [ A.classList [ ("person", True)
                         , ("in-group", index.byPerson id /= Nothing)
                         , ("no-group", noMatchingGroups)
                         , ("asgl", person.role == Person.Asgl)
                         , ("highlight", highlighted)
                         ]
           , onMouseOver (Hover <| HoverPerson id)
           , onMouseOut (Hover <| NoHover)
           ] [ span [ draggable True
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
        , viewPeople model.hover index "ASGLs" leaders
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
    in \timeslot -> Maybe.withDefault 0 <| Dict.get (toString timeslot) values


peoplePerTimeslot : List Person -> Timeslot -> List Person
peoplePerTimeslot people =
    let addValue person = (Maybe.withDefault []) >> ((::) person) >> Just
        insertTimeslot person timeslot = Dict.update (toString timeslot) (addValue person)
        addPerson person dict = List.foldl (insertTimeslot person) dict person.free
        values = List.foldl addPerson Dict.empty people
    in \timeslot -> Maybe.withDefault [] <| Dict.get (toString timeslot) values


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


asglFirst : Person -> Person -> Order
asglFirst left right =
    case (left.role, right.role) of
        (Person.Asgl, Person.Member) ->
            LT

        (Person.Member, Person.Asgl) ->
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
            if leaderCount < 2 then
                "#000"
            else
                weight timeslot |> valueToBackgroundColor
        foreground =
            if leaderCount < 2 then
                "#000"
            else
                weight timeslot |> valueToTextColor
        lookupPerson id =
            case Dict.get id people of
                Just person ->
                    [(id, person)]
                Nothing ->
                    []

        mGroup = index.byTimeslot timeslot

        groupPeople = Maybe.withDefault Set.empty <| Maybe.map (.people << snd) mGroup

        hasGroup = mGroup /= Nothing

        highlighted = case hover of
                          HoverPerson id ->
                              Dict.get id people
                                  |> Maybe.map (flip freeAt timeslot)
                                  |> Maybe.withDefault False

                          _ ->
                              False
    in
        td [ A.style [ ("background-color", background)
                     , ("color", foreground)
                     ]
           , A.classList [ ("has-group", hasGroup)
                         , ("highlight", highlighted)
                         ]
           , dropTarget True
           , onDrop Person.idDrag (Add timeslot)
           -- , onDrop Group.idDrag (flip Move timeslot)
           , onMouseOver (Hover <| HoverTimeslot timeslot)
           , onMouseOut (Hover <| NoHover)
           , onClick (Delete timeslot)
           ] (if Set.isEmpty groupPeople then
                  [ text (toString leaderCount ++ " / " ++ toString memberCount)
                  ]
              else
                  (groupPeople
                  |> Set.toList
                  |> List.concatMap lookupPerson
                  |> List.sortWith (sortSecond asglFirst)
                  |> List.map (viewPerson hover { index | byPerson = always Nothing })))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
