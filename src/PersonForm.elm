module PersonForm exposing (Model, Msg, ParentMsg(..), init, update, view, extract)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Data.Person as Person exposing (Person, Gender(..), Role(..))
import Data.Timeslot as Timeslot exposing (Timeslot, Day(..), Time)
import Util exposing ((!!))

type Msg = UpdateName String
         | UpdateRole String
         | UpdateGender String
         | UpdateFree Timeslot Bool

type ParentMsg = None
               | Save Person

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
        "Leader" -> Just Leader
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
    case msg of
        UpdateName value ->
            let person' = { person | name = value }
            in person' ! [] !! Save person'

        UpdateRole input ->
            case Debug.log "Bleep" <| roleFromString input of
                Just role ->
                    let person' = { person | role = role }
                    in person' ! [] !! Save person'

                Nothing ->
                    person ! [] !! None

        UpdateGender input ->
            case Debug.log "bloop" <| genderFromString input of
                Just gender ->
                    let person' = { person | gender = gender }
                    in person' ! [] !! Save person'

                Nothing ->
                    person ! [] !! None

        UpdateFree timeslot free ->
            if free then
                if person `freeAt` timeslot then
                    person ! [] !! None
                else 
                    let person' = { person | free = timeslot :: person.free }
                    in person' ! [] !! Save person'
            else
                let person' = { person | free = List.filter ((/=) timeslot) person.free }
                in person' ! [] !! Save person'


view : Model -> Html Msg
view person =
    Html.div [ ]
        [ viewName person
        , viewRole person
        , viewGender person
        , viewFree person
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
        select [ on "change" (Json.map UpdateRole targetValue) ] (List.map opt Person.roles)

viewGender : Person -> Html Msg
viewGender person =
    let
        opt : Person.Gender -> Html Msg
        opt gender = option [ value (toString gender)
                            , selected (person.gender == gender)
                            ] [ text (toString gender) ]
    in
        select [ on "change" (Json.map UpdateGender targetValue) ] (List.map opt Person.genders)

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
    
        
