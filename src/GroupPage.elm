module GroupPage exposing (Model, Msg, ParentMsg(..), init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onMouseOver, onMouseOut)
import Dict exposing (Dict)
import Set exposing (Set)
import Data.Group as Group exposing (Group)
import Data.Person as Person exposing (Person)
import Data.Timeslot as Timeslot exposing (Timeslot, Day(..))
import Util exposing ((!!), hex, idGenerator)
import DragAndDrop exposing (draggable, dragData, dropTarget, onDrop)
import Validation exposing (validateGroup)
import String


type HoverState
    = HoverPerson Person.Id
    | HoverGroup Timeslot
    | NoHover


type alias Model =
    { groups : Group.Dict
    , people : Person.Dict
    , hover : HoverState
    }


type Msg
    = Move Person.Id Group.Id
    | Remove Person.Id
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


fixGroup : Group.Id -> Person.Id -> Group.Id -> Group -> Group
fixGroup targetId personId groupId group =
    let people' = if groupId == targetId then
                      Set.insert personId group.people
                  else
                      Set.remove personId group.people
    in { group | people = people' }


update : Msg -> Model -> (Model, Cmd Msg, ParentMsg)
update msg model =
    let index = Group.makeIndex model.groups
    in case msg of
           Move personId groupId ->
               let groups' = Dict.map (fixGroup groupId personId) model.groups
               in { model | groups = groups' } ! [] !! SaveGroups groups'

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

           Hover state ->
               { model | hover = state } ! [] !! None


hasRole : Person.Role -> Person.Id -> Person -> Bool
hasRole role id person = person.role == role


splitRoles : Person.Dict -> {leaders : Person.Dict, members : Person.Dict}
splitRoles people =
    { leaders = Dict.filter -- (hasRole Person.Asgl)
          (always <| (==) Person.Asgl << .role)
          people
    , members = Dict.filter (hasRole Person.Member) people
    }


viewPerson : HoverState -> Group.Index -> (Person.Id, Person) -> Html Msg
viewPerson hover index (id, person) =
    let noMatchingGroups = person.free
                         |> List.map index.byTimeslot
                         |> List.all ((==) Nothing)
        highlighted = case hover of
                          HoverGroup timeslot ->
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


viewPeople : HoverState -> Group.Index -> String -> Person.Dict -> Html Msg
viewPeople hover index title people =
    div [ A.class "person-list"
        , dropTarget True
        , onDrop Person.idDrag Remove
        ] (h1 [] [ text title ] ::
               (Dict.toList people
               |> List.sortBy (.name << snd)
               |> List.map (viewPerson hover index)))


viewGroup : HoverState -> Group.Index -> Person.Dict -> (Group.Id, Group) -> Html Msg
viewGroup hover index people (id, group) =
    let
        members = people
                |> Dict.filter (\id _ -> Set.member id group.people)

        name = Timeslot.toString group.time

        highlighted = case hover of
                          HoverPerson hoverId ->
                              Dict.get hoverId people
                                  |> Maybe.map (flip freeAt group.time)
                                  |> Maybe.withDefault False

                          _ ->
                              False

        groupErrors = validateGroup people group

        (hasErrors, errors) =
            case groupErrors of
                Err errors -> (True, errors)
                _ -> (False, [])
    in
        div [ A.classList [ ("group", True)
                          , ("highlight", highlighted)
                          , ("error", hasErrors)
                          ]
            , A.title (String.join "\n" errors)
            , dropTarget True
            , onDrop Person.idDrag (flip Move id)
            , onMouseOver (Hover <| HoverGroup group.time)
            , onMouseOut (Hover <| NoHover)
            ] (h1 [] [ text name ] ::
                   (Dict.toList members
                   |> List.sortWith (\a b -> asglFirst (snd a) (snd b))
                   |> List.map (viewPerson hover { index | byPerson = always Nothing })))


compareGroups : (Group.Id, Group) -> (Group.Id, Group) -> Order
compareGroups (lid, left) (rid, right) =
    Timeslot.compare left.time right.time


viewGroups : HoverState -> Group.Index -> Person.Dict -> Group.Dict -> Html Msg
viewGroups hover index people groups =
    div [ A.class "group-list" ]
        (Dict.toList groups
        |> List.sortWith compareGroups
        |> List.map (viewGroup hover index people))


view : Model -> Html Msg
view {hover, people, groups} =
    let {leaders, members} = splitRoles people
        index = Group.makeIndex groups
    in div [ A.class "group-page" ]
        [ viewGroups hover index people groups
        , viewPeople hover index "ASGLs" leaders
        , viewPeople hover index "Members" members
        ]


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


-- viewGroup : HoverState -> Person.Dict -> Group -> Html Msg
-- viewGroup hover people group =
--     let
--         fullPeople =
--             List.filterMap (flip Dict.get people) group.people

--         name = (toString <| fst group) ++ "@" ++ (toString <| snd group)
--     in
--         div []
        


-- viewTimeslot : HoverState -> Person.Dict -> TimeslotFunctions -> Timeslot -> Html Msg
-- viewTimeslot hover people {leaders, members, weight, index} timeslot =
--     let
--         leaderCount =
--             leaders timeslot |> List.length
--         memberCount =
--             members timeslot |> List.length
--         background =
--             if leaderCount < 2 then
--                 "#000"
--             else
--                 weight timeslot |> valueToBackgroundColor
--         foreground =
--             if leaderCount < 2 then
--                 "#000"
--             else
--                 weight timeslot |> valueToTextColor
--         lookupPerson id =
--             case Dict.get id people of
--                 Just person ->
--                     [(id, person)]
--                 Nothing ->
--                     []

--         mGroup = index.byTimeslot timeslot

--         groupPeople = Maybe.withDefault Set.empty <| Maybe.map (.people << snd) mGroup

--         hasGroup = mGroup /= Nothing

--         highlighted = case hover of
--                           HoverPerson id ->
--                               Dict.get id people
--                                   |> Maybe.map (flip freeAt timeslot)
--                                   |> Maybe.withDefault False

--                           _ ->
--                               False
--     in
--         td [ A.style [ ("background-color", background)
--                      , ("color", foreground)
--                      ]
--            , A.classList [ ("has-group", hasGroup)
--                          , ("highlight", highlighted)
--                          ]
--            , dropTarget True
--            , onDrop Person.idDrag (Add timeslot)
--            -- , onDrop Group.idDrag (flip Move timeslot)
--            ] (if Set.isEmpty groupPeople then
--                   [ text (toString leaderCount ++ " / " ++ toString memberCount)
--                   ]
--               else
--                   (groupPeople
--                   |> Set.toList
--                   |> List.concatMap lookupPerson
--                   |> List.sortWith (sortSecond asglFirst)
--                   |> List.map (viewPerson hover { index | byPerson = always Nothing })))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
