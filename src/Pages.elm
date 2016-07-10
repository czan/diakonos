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
import GroupPage

type PageName = People
              | Groups

type Msg = PersonMsg PersonPage.Msg
         | GroupMsg GroupPage.Msg
         | ChangePage PageName
         | KeyDown Keyboard.KeyCode
         | KeyUp Keyboard.KeyCode

type ParentMsg = None

type PageModel = Person PersonPage.Model
               | Group GroupPage.Model

type alias Model =
    { page : PageModel
    , people : Dict Person.Id Person
    , groups : Dict Group.Id Group
    , ctrlDown : Bool
    }

init : Person.Dict -> Group.Dict -> (Model, Cmd Msg)
init people groups =
    let (model, cmd) = PersonPage.init people
    in { page = Person model
       , people = people
       , groups = groups
       , ctrlDown = False
       } ! [ Cmd.map PersonMsg cmd ]

updatePerson : PersonPage.ParentMsg -> Model -> Model
updatePerson msg model =
    case msg of
        PersonPage.SavePerson id person ->
            { model | people = Dict.insert id person model.people }

        PersonPage.None ->
            model

updateGroup : GroupPage.ParentMsg -> Model -> Model
updateGroup msg model = model

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
        Group _ -> changePage People model

changePageRight : Model -> (Model, Cmd Msg)
changePageRight = changePageLeft -- they're the same if there are only two pages

changePage : PageName -> Model -> (Model, Cmd Msg)
changePage page model =
    case page of
        People ->
            let (pmodel, pcmd) = PersonPage.init model.people
            in { model | page = Person pmodel } ! [ Cmd.map PersonMsg pcmd ]

        Groups ->
            let (pmodel, pcmd) = GroupPage.init model.people model.groups
            in { model | page = Group pmodel } ! [ Cmd.map GroupMsg pcmd ]
                     
update : Msg -> Model -> (Model, Cmd Msg, ParentMsg)
update msg model =
    case (msg, model.page) of
        (PersonMsg pmsg, Person pmodel) ->
            let (pmodel, pcmd, parentMsg) = PersonPage.update pmsg pmodel
                model' = updatePerson parentMsg model
            in { model' | page = Person pmodel } ! [ Cmd.map PersonMsg pcmd ] !! None

        (PersonMsg _, _) ->
            model ! [] !! None

        (GroupMsg pmsg, Group pmodel) ->
            let (pmodel, pcmd, parentMsg) = GroupPage.update pmsg pmodel
                model' = updateGroup parentMsg model
            in { model' | page = Group pmodel } ! [ Cmd.map GroupMsg pcmd ] !! None

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
              , viewNavLink Groups model.page
              ]
        , viewPage model
        ]

viewNavLink : PageName -> PageModel -> Html Msg
viewNavLink page model =
    let selected = case (model, page) of
                       (Person _, People) -> True
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
            App.map PersonMsg (PersonPage.view pmodel)

        Group gmodel ->
            App.map GroupMsg (GroupPage.view gmodel)

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
            Sub.map PersonMsg (PersonPage.subscriptions pmodel)

        Group gmodel ->
            Sub.map GroupMsg (GroupPage.subscriptions gmodel)
