module Pages exposing (Msg, ParentMsg(..), Model, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Dict exposing (Dict)
import Data.Person as Person exposing (Person)
import Data.Group as Group exposing (Group)
import Keyboard
import Util exposing ((!!))

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

type ParentMsg = None

type PageModel = Person PersonPage.Model
               | Time TimePage.Model
               | Group GroupPage.Model

type alias Model =
    { page : PageModel
    , people : Person.Dict
    , groups : Group.Dict
    , ctrlDown : Bool
    }

init : Person.Dict -> Group.Dict -> (Model, Cmd Msg)
init people groups =
    let
        -- (model, cmd) = PersonPage.init people
        -- (model, cmd) = TimePage.init people groups
        (model, cmd) = GroupPage.init people groups
    in { page = Group model
       , people = people
       , groups = groups
       , ctrlDown = False
       } ! [ Cmd.map GroupMsg cmd ]

updatePerson : PersonPage.ParentMsg -> Model -> (Model, Cmd Msg, ParentMsg)
updatePerson msg model =
    case msg of
        PersonPage.SavePerson id person ->
            let people' = Dict.insert id person model.people
            in { model | people = people' } ! [] !! None

        PersonPage.None ->
            model ! [] !! None

updateTime : TimePage.ParentMsg -> Model -> (Model, Cmd Msg, ParentMsg)
updateTime msg model =
    case msg of
        TimePage.SaveGroups groups ->
            let groups' = groups
            in { model | groups = groups } ! [] !! None

        TimePage.None ->
            model ! [] !! None


updateGroup : GroupPage.ParentMsg -> Model -> (Model, Cmd Msg, ParentMsg)
updateGroup msg model =
    case msg of
        GroupPage.SaveGroups groups ->
            let groups' = groups
            in { model | groups = groups } ! [] !! None

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

changePage : PageName -> Model -> (Model, Cmd Msg)
changePage page model =
    case page of
        People ->
            let (pmodel, pcmd) = PersonPage.init model.people
            in { model | page = Person pmodel } ! [ Cmd.map PersonMsg pcmd ]

        Times ->
            let (pmodel, pcmd) = TimePage.init model.people model.groups
            in { model | page = Time pmodel } ! [ Cmd.map TimeMsg pcmd ]

        Groups ->
            let (pmodel, pcmd) = GroupPage.init model.people model.groups
            in { model | page = Group pmodel } ! [ Cmd.map GroupMsg pcmd ]
                     
update : Msg -> Model -> (Model, Cmd Msg, ParentMsg)
update msg model =
    case (msg, model.page) of
        (PersonMsg pmsg, Person pmodel) ->
            let (pmodel', pcmd, parentMsg) = PersonPage.update pmsg pmodel
                (model', cmd, msg) = updatePerson parentMsg model
            in { model' | page = Person pmodel' } ! [ Cmd.map PersonMsg pcmd, cmd ] !! msg

        (PersonMsg _, _) ->
            model ! [] !! None

        (TimeMsg pmsg, Time pmodel) ->
            let (pmodel', pcmd, parentMsg) = TimePage.update pmsg pmodel
                (model', cmd, msg) = updateTime parentMsg model
            in { model' | page = Time pmodel' } ! [ Cmd.map TimeMsg pcmd, cmd ] !! msg

        (TimeMsg _, _) ->
            model ! [] !! None

        (GroupMsg pmsg, Group pmodel) ->
            let (pmodel', pcmd, parentMsg) = GroupPage.update pmsg pmodel
                (model', cmd, msg) = updateGroup parentMsg model
            in { model' | page = Group pmodel' } ! [ Cmd.map GroupMsg pcmd, cmd ] !! msg

        (GroupMsg _, _) ->
            model ! [] !! None

        (ChangePage page, _) ->
            changePage page model !! None

        (KeyDown code, _) ->
            updateKeydown code model !! None

        (KeyUp code, _) ->
            updateKeyup code model ! [] !! None

view : Model -> Html Msg
view model =
    div [ id "container" ]
        [ div [class "navigation"]
              [ viewNavLink People model.page
              , viewNavLink Times model.page
              , viewNavLink Groups model.page
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
        ]

topLevelSubscriptions : Model -> Sub Msg
topLevelSubscriptions model =
    if model.ctrlDown then
        Sub.batch [ Keyboard.downs KeyDown
                  , Keyboard.ups KeyUp
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
