module PersonPage exposing (Model, Msg, ParentMsg(..), init, update, view, subscriptions)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Data.Person as Person exposing (Person, Role(..), Gender(..))
import PersonForm as PersonForm
import Dict exposing (Dict)
import Random
import Char
import String
import Keyboard
import Util exposing ((!!), dropWhile, takeWhile)
import Ports exposing (scrollToVisible)

type alias Model =
    { people : Dict Person.Id PersonForm.Model
    , selected : Maybe Person.Id
    , form : PersonForm.Model
    , altDown : Bool
    }

type Msg = Select (Maybe Person.Id)
         | Add Person
         | Save Person.Id Person
         | FormMsg PersonForm.Msg
         | KeyUp Keyboard.KeyCode
         | KeyDown Keyboard.KeyCode

type ParentMsg = SavePerson Person.Id Person
               | None

toHex : Int -> Char
toHex i = Char.fromCode (if i < 10 then
                             48 + i
                         else
                             97 + (i - 10))
           
numListToHex : List Int -> String
numListToHex l = String.join "" (List.map (String.fromChar << toHex) l)
           
idGenerator : Random.Generator Person.Id
idGenerator = Random.map numListToHex (Random.list 20 (Random.int 0 15))

init : Dict String Person -> ( Model, Cmd Msg )
init people =
    ( { people = people
      , selected = Nothing
      , form = PersonForm.init Nothing
      , altDown = False
      }
    , Cmd.none )

moveSelectionDown : Model -> (Model, Cmd a)
moveSelectionDown model =
    let pid (id, _) = id
        selection = case model.selected of
                        Just id ->
                            people model
                                |> List.map pid
                                |> dropWhile ((/=) id)
                                |> List.head

                        Nothing ->
                            people model
                                |> List.map pid
                                |> List.head
    in updateSelection selection model

moveSelectionUp : Model -> (Model, Cmd a)
moveSelectionUp model =
    let pid (id, _) = id
        selection = case model.selected of
                        Just id ->
                            people model
                                |> List.map pid
                                |> takeWhile ((/=) id)
                                |> List.reverse
                                |> List.head
                                   
                        Nothing ->
                            people model
                                |> List.map pid
                                |> List.reverse
                                |> List.head
    in updateSelection selection model

update : Msg -> Model -> (Model, Cmd Msg, ParentMsg)
update msg model =
    case msg of
        FormMsg msg ->
            let (form,  cmd,  fromForm) = PersonForm.update msg model.form
                (model, cmd2, parentMsg) = updateFromForm fromForm { model | form = form }
            in model ! [cmd, cmd2] !! parentMsg

        KeyUp key ->
            updateKeyup key model ! [] !! None

        KeyDown key ->
            updateKeydown key model !! None

        Select id ->
            updateSelection id model !! None

        Add person ->
            model ! [Random.generate (\i -> Save i person) idGenerator] !! None

        Save id person ->
            updateSelection (Just id) { model | people = Dict.insert id person model.people } !! SavePerson id person
            -- { model
            --     | selected = Just id
            --     , people = Dict.insert id person model.people
            -- } ! [] !! SavePerson id person

updateSelection : Maybe Person.Id -> Model -> (Model, Cmd a)
updateSelection mid model =
    case mid of
        Just id ->
            case Dict.get id model.people of
                Just person ->
                    { model
                        | selected = Just id
                        , form = PersonForm.init (Just person)
                    } ! [ scrollToVisible ("person-" ++ id) ]
                Nothing -> model ! []
        Nothing ->
            { model | selected = Nothing, form = PersonForm.init Nothing } ! [ scrollToVisible "new-person" ]

updateKeyup : Keyboard.KeyCode -> Model -> Model
updateKeyup key model =
    case key of
        18 -> { model | altDown = False }
        _  -> model

updateKeydown : Keyboard.KeyCode -> Model -> (Model, Cmd a)
updateKeydown key model =
    case key of
        18 -> { model | altDown = True } ! []
        38 -> if model.altDown then moveSelectionUp model else (model ! [])
        40 -> if model.altDown then moveSelectionDown model else (model ! [])
        _  -> model ! []

updateFromForm : PersonForm.ParentMsg -> Model -> (Model, Cmd Msg, ParentMsg)
updateFromForm msg model =
    case msg of
        PersonForm.Save person ->
            case model.selected of
                Just id ->
                    update (Save id person) model

                Nothing ->
                    update (Add person) model

        PersonForm.Revert ->
            case model.selected of
                Just id ->
                    case Dict.get id model.people of
                        Just person ->
                            { model | form = PersonForm.init (Just person) } ! [] !! None

                        Nothing -> model ! [] !! None

                Nothing -> model ! [] !! None

        PersonForm.None -> model ! [] !! None

view : Model -> Html Msg
view model =
    div [ class "person-page" ]
        [ viewList model
        , viewForm model]

people : Model -> List (Person.Id, Person)
people model =
    let sorted = model.people
               |> Dict.toList
               |> List.sortBy (\ (id, p) -> p.name )
        roleIs value (_, person) = (value == person.role)
        asgls = List.filter (roleIs Person.Asgl) sorted
        members = List.filter (roleIs Person.Member) sorted
    in asgls ++ members

viewList : Model -> Html Msg
viewList model =
    let selected id = case (id, model.selected) of
                          (Just id, Just sel) -> id == sel
                          (Nothing, Nothing) -> True
                          _ -> False
        nullItem = div [ onClick (Select Nothing)
                       , classList [ ("person-label", True)
                                   , ("selected", selected Nothing)]
                       , id "new-person"
                       ] [ text "[new person]" ]
        item (pid, person) = div [ onClick (Select (Just pid))
                                 , classList [ ("person-label", True)
                                             , ("selected", selected (Just pid))]
                                 , id ("person-" ++ pid)
                                 ] [text person.name]
        roleIs value (_, person) = (value == person.role)
        asglList = h1 [] [ text "ASGLs"] :: (people model
                                            |> List.filter (roleIs Person.Asgl)
                                            |> List.map item)
        memberList = h1 [] [ text "Members"] :: (people model
                                                |> List.filter (roleIs Person.Member)
                                                |> List.map item)
    in div [class "person-list"]
        ([nullItem] ++ asglList ++ memberList)

viewForm : Model -> Html Msg
viewForm model =
    div [ class "form" ]
        [ (App.map FormMsg (PersonForm.view model.form)) ]


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.altDown then
        Sub.batch [ Keyboard.downs KeyDown
                  , Keyboard.ups KeyUp
                  ]
    else
        Keyboard.downs KeyDown
