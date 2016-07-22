module Pages exposing (Msg, ParentMsg(..), Model, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Data.Person as Person exposing (Person)
import Data.Group as Group exposing (Group)
import Keyboard
import Util exposing ((!!))
import Dict
import Json.Encode as E
import Json.Decode as D exposing ((:=))
import Ports

import PersonPage
import TimePage
import GroupPage

type PageName = People
              | Times
              | Groups

type Msg = PersonMsg PersonPage.Msg
         | TimeMsg TimePage.Msg
         | GroupMsg GroupPage.Msg
         | ChangePage PageName
         | KeyDown Keyboard.KeyCode
         | KeyUp Keyboard.KeyCode
         | Save
         | Load
         | LoadData Person.Dict Group.Dict
         | LoadFail String

type ParentMsg = SaveData Person.Dict Group.Dict
               | None

type PageModel = Person PersonPage.Model
               | Time TimePage.Model
               | Group GroupPage.Model

type alias PersistedModels =
    { person : Maybe PersonPage.PersistentModel
    }

type alias Model =
    { page : PageModel
    , people : Person.Dict
    , groups : Group.Dict
    , ctrlDown : Bool
    , persistedModels : PersistedModels
    }

init : Person.Dict -> Group.Dict -> (Model, Cmd Msg)
init people groups =
    let
        (model, cmd) = PersonPage.init Nothing people
        -- (model, cmd) = TimePage.init people groups
        -- (model, cmd) = GroupPage.init people groups
    in { page = Person model
       , people = people
       , groups = groups
       , ctrlDown = False
       , persistedModels = { person = Nothing }
       } ! [ Cmd.map PersonMsg cmd ]

reInit : Person.Dict -> Group.Dict -> Model -> (Model, Cmd Msg)
reInit people groups model =
    case model.page of
        Person _ ->
            changePage People { model | people = people, groups = groups }
        Time _ ->
            changePage Times { model | people = people, groups = groups }
        Group _ ->
            changePage Groups { model | people = people, groups = groups }

updatePerson : PersonPage.ParentMsg -> Model -> (Model, Cmd Msg, ParentMsg)
updatePerson msg model =
    case msg of
        PersonPage.SavePeople people ->
            { model | people = people } ! [] !! SaveData people model.groups

        PersonPage.None ->
            model ! [] !! None

updateTime : TimePage.ParentMsg -> Model -> (Model, Cmd Msg, ParentMsg)
updateTime msg model =
    case msg of
        TimePage.SaveGroups groups ->
            { model | groups = groups } ! [] !! SaveData model.people groups

        TimePage.None ->
            model ! [] !! None


updateGroup : GroupPage.ParentMsg -> Model -> (Model, Cmd Msg, ParentMsg)
updateGroup msg model =
    case msg of
        GroupPage.SaveGroups groups ->
            { model | groups = groups } ! [] !! SaveData model.people groups

        GroupPage.None ->
            model ! [] !! None


updateKeyup : Keyboard.KeyCode -> Model -> Model
updateKeyup key model =
    case key of
        17 -> { model | ctrlDown = False }
        _ -> model

updateKeydown : Keyboard.KeyCode -> Model -> (Model, Cmd Msg)
updateKeydown key model =
    case key of
        17 -> { model | ctrlDown = True } ! []
        37 -> if model.ctrlDown then changePageLeft model else (model ! [])
        39 -> if model.ctrlDown then changePageRight model else (model ! [])
        -- 83 -> if model.ctrlDown then (update Save model) else (model ! [])
        _ -> model ! []

changePageLeft : Model -> (Model, Cmd Msg)
changePageLeft model =
    case model.page of
        Person _ -> changePage Groups model
        Time _ -> changePage People model
        Group _ -> changePage Times model

changePageRight : Model -> (Model, Cmd Msg)
changePageRight model =
    case model.page of
        Person _ -> changePage Times model
        Time _ -> changePage Groups model
        Group _ -> changePage People model

persist : PageModel -> PersistedModels -> PersistedModels
persist model persisted =
    case model of
        Person model ->
            { persisted | person = Just (PersonPage.persist model) }

        _ ->
            persisted
            

changePage : PageName -> Model -> (Model, Cmd Msg)
changePage page model =
    let persisted = persist model.page model.persistedModels
    in case page of
        People ->
            let (pmodel, pcmd) = PersonPage.init model.persistedModels.person model.people
                model' = { model | page = Person pmodel, persistedModels = persisted }
            in model' ! [ Cmd.map PersonMsg pcmd ]

        Times ->
            let (pmodel, pcmd) = TimePage.init model.people model.groups
                model' = { model | page = Time pmodel, persistedModels = persisted }
            in model' ! [ Cmd.map TimeMsg pcmd ]

        Groups ->
            let (pmodel, pcmd) = GroupPage.init model.people model.groups
                model' = { model | page = Group pmodel, persistedModels = persisted }
            in model' ! [ Cmd.map GroupMsg pcmd ]

updatePersonMsg : PersonPage.Msg -> Model -> (Model, Cmd Msg)
updatePersonMsg pmsg model =
    case model.page of
        Person pmodel ->
            let (pmodel', pcmd, parentMsg) = PersonPage.update pmsg pmodel
                (model', cmd, msg) = updatePerson parentMsg model
            in { model' | page = Person pmodel' } ! [ Cmd.map PersonMsg pcmd, cmd ]

        _ ->
            model ! []

updateTimeMsg : TimePage.Msg -> Model -> (Model, Cmd Msg)
updateTimeMsg pmsg model =
    case model.page of
        Time pmodel ->
            let (pmodel', pcmd, parentMsg) = TimePage.update pmsg pmodel
                (model', cmd, msg) = updateTime parentMsg model
            in { model' | page = Time pmodel' } ! [ Cmd.map TimeMsg pcmd, cmd ]

        _ ->
            model ! []

updateGroupMsg : GroupPage.Msg -> Model -> (Model, Cmd Msg)
updateGroupMsg pmsg model =
    case model.page of
        Group pmodel ->
            let (pmodel', pcmd, parentMsg) = GroupPage.update pmsg pmodel
                (model', cmd, msg) = updateGroup parentMsg model
            in { model' | page = Group pmodel' } ! [ Cmd.map GroupMsg pcmd, cmd ]

        _ ->
            model ! []

                     
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Save ->
            let people = Dict.map (always Person.encode) model.people
                       |> Dict.toList
                       |> E.object
                groups = Dict.map (always Group.encode) model.groups
                       |> Dict.toList
                       |> E.object
                data = E.object [ ("people", people)
                                   , ("groups", groups)
                                   ]
            in model ! [ Ports.save <| E.encode 2 data ]

        Load ->
            model ! [ Ports.load () ]

        LoadData people groups ->
            reInit people groups model

        LoadFail _ ->
            model ! []

        PersonMsg pmsg ->
            updatePersonMsg pmsg model

        TimeMsg pmsg ->
            updateTimeMsg pmsg model

        GroupMsg pmsg ->
            updateGroupMsg pmsg model

        ChangePage page ->
            changePage page model

        KeyDown code ->
            updateKeydown code model

        KeyUp code ->
            updateKeyup code model ! []

view : Model -> Html Msg
view model =
    div [ id "container" ]
        [ div [class "navigation"]
              [ viewNavLink People model.page
              , viewNavLink Times model.page
              , viewNavLink Groups model.page
              , viewLink "Save" Save
              , viewLink "Load" Load
              ]
        , viewPage model
        ]

viewNavLink : PageName -> PageModel -> Html Msg
viewNavLink page model =
    let selected = case (model, page) of
                       (Person _, People) -> True
                       (Time _, Times) -> True
                       (Group _, Groups) -> True
                       _ -> False
    in
        div [ classList [ ("selected", selected) ]
            , onClick (ChangePage page)
            ] [ text (toString page) ]


viewLink : String -> Msg -> Html Msg
viewLink str msg =
    div [ class "management"
        , onClick msg
        ] [ text str ]


viewPage : Model -> Html Msg
viewPage model =
    case model.page of
        Person pmodel ->
            App.map PersonMsg <| PersonPage.view pmodel

        Time pmodel ->
            App.map TimeMsg <| TimePage.view pmodel

        Group pmodel ->
            App.map GroupMsg <| GroupPage.view pmodel

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pageSubscriptions model
        , topLevelSubscriptions model
        , Ports.loadedData decodeValue
        ]

topLevelSubscriptions : Model -> Sub Msg
topLevelSubscriptions model =
    if model.ctrlDown then
        Sub.batch [ Keyboard.downs KeyDown
                  , Keyboard.ups KeyUp
                  , Ports.loadedData decodeValue
                  ]
    else
        Keyboard.downs KeyDown


pageSubscriptions : Model -> Sub Msg
pageSubscriptions model =
    case model.page of
        Person pmodel ->
            Sub.map PersonMsg <| PersonPage.subscriptions pmodel

        Time pmodel ->
            Sub.map TimeMsg <| TimePage.subscriptions pmodel

        Group pmodel ->
            Sub.map GroupMsg <| GroupPage.subscriptions pmodel

decodeValue : E.Value -> Msg
decodeValue value =
    case D.decodeValue requestDecoder value of
        Ok (people, groups) ->
            LoadData people groups
        Err _ ->
            LoadFail "Couldn't read file!"

requestDecoder : D.Decoder (Person.Dict, Group.Dict)
requestDecoder =
    D.object2 (,)
        ("people" := D.dict Person.decode)
        ("groups" := D.dict Group.decode)
