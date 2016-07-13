module PersonForm exposing (Model, Msg, ParentMsg(..), init, update, view, extract)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Data.Person as Person exposing (Person, Gender(..), Role(..))
import Data.Timeslot as Timeslot exposing (Timeslot, Day(..), Time)

type Msg = UpdateName String
         | UpdateRole String
         | UpdateGender String
         | UpdateFree Timeslot Bool
         | SaveForm
         | RevertForm

type ParentMsg = None
               | Save Person
               | Revert

type alias Model = Person

init : Maybe Person -> Model
init person =
    case person of
        Just person -> person
        Nothing -> { name = ""
                   , role = Member
                   , gender = Male
                   , free = []
                   }

extract : Model -> Person
extract person = person

roleFromString : String -> Maybe Role
roleFromString input =
    case input of
        "Howie" -> Just Howie
        "Asgl" -> Just Asgl
        "Member" -> Just Member
        _ -> Nothing

genderFromString : String -> Maybe Gender
genderFromString input =
    case input of
        "Male" -> Just Male
        "Female" -> Just Female
        _ -> Nothing
                 
update : Msg -> Model -> (Model, Cmd a, ParentMsg)
update msg person =
    let parentMsg = case msg of
                        SaveForm -> Save person
                        RevertForm -> Revert
                        _ -> None
    in
        ( simpleUpdate msg person
        , Cmd.none
        , parentMsg)

simpleUpdate : Msg -> Model -> Model
simpleUpdate msg person =
    case msg of
        SaveForm ->
            person

        RevertForm ->
            person

        UpdateName value ->
            { person | name = value }

        UpdateRole input ->
            case roleFromString input of
                Just role ->
                    { person | role = role }

                Nothing -> person

        UpdateGender input ->
            case genderFromString input of
                Just gender ->
                    { person | gender = gender }

                Nothing -> person

        UpdateFree timeslot free ->
            if free then
                if person `freeAt` timeslot then
                    person
                else 
                    { person | free = timeslot :: person.free }
            else
                { person | free = List.filter ((/=) timeslot) person.free }

view : Model -> Html Msg
view person =
    Html.form [ onSubmit SaveForm ]
        [ viewName person
        , viewRole person
        , viewGender person
        , viewFree person
        , viewButtons
        ]

viewButtons : Html Msg
viewButtons =
    div []
        [ input [ type' "submit", value "Save" ] []
        , button [ onClick RevertForm ] [ text "Revert" ]
        ]

viewName : Person -> Html Msg
viewName person =
    div []
        [ input [ type' "text"
                , onInput UpdateName
                , value person.name
                ] []
        ]

viewRole : Person -> Html Msg
viewRole person =
    let
        opt : Person.Role -> Html Msg
        opt role = option [ value (toString role)
                          , selected (person.role == role)
                          ] [ text (toString role) ]
    in
        select [ onInput UpdateRole ] (List.map opt Person.roles)

viewGender : Person -> Html Msg
viewGender person =
    let
        opt : Person.Gender -> Html Msg
        opt gender = option [ value (toString gender)
                            , selected (person.gender == gender)
                            ] [ text (toString gender) ]
    in
        select [ onInput UpdateGender ] (List.map opt Person.genders)

freeAt : Person -> Timeslot -> Bool
freeAt person timeslot =
    List.member timeslot person.free

freetimeslot : Person -> Timeslot -> Html Msg
freetimeslot person timeslot =
    let free = freeAt person timeslot
    in td []
        [ div [ classList [ ("free", free)
                          , ("busy", not free)
                          ]
              , onClick (UpdateFree timeslot <| not free)
              ] [ ]
        ]

viewFree : Person -> Html Msg
viewFree person =
    Timeslot.table [] (freetimeslot person)
    
        
