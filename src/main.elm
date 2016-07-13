import Html.App as App
import Pages
import Dict exposing (Dict)
import Data.Person as Person exposing (Person)
import Data.Group as Group exposing (Group)
import Html exposing (..)
import Json.Decode as Json exposing ((:=))
import Task
import Http


type Msg
    = RestoreData (Person.Dict, Group.Dict)
    | RestoreFail Http.Error
    | PageMsg Pages.Msg


type State
    = Loaded Pages.Model
    | Loading
    | Failed


type alias Model =
    { people : Person.Dict
    , groups : Group.Dict
    , state : State
    }


main : Program Never
main =
    App.program
        { init = init "http://localhost:8080/data.json"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : String -> (Model, Cmd Msg)
init url =
    let request = url
                |> Http.get (Json.object2 (,)
                                 ("people" := Json.dict Person.decode)
                                 ("groups" := Json.dict Group.decode))
    in { people = Dict.empty
       , groups = Dict.empty
       , state = Loading
       } ! [ Task.perform RestoreFail RestoreData request ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg, model.state) of
        (PageMsg pmsg, Loaded pmodel) ->
            let (pmodel', pcmd, parentMsg) = Pages.update pmsg pmodel
                (model', cmd) = updateFromPages parentMsg model
            in { model' | state = Loaded pmodel' } ! [ Cmd.map PageMsg pcmd, cmd ]

        (RestoreData (people, groups), Loading) ->
            let (pmodel, pcmd) = Pages.init people groups
            in { people = people
               , groups = groups
               , state = Loaded pmodel
               } ! [ Cmd.map PageMsg pcmd ]

        (_, Loaded _) ->
            model ! []

        (_, Loading) ->
            model ! []

        (_, Failed) ->
            model ! []
            

updateFromPages : Pages.ParentMsg -> Model -> (Model, Cmd Msg)
updateFromPages msg model =
    case msg of
        Pages.None ->
            model ! []


view : Model -> Html Msg
view model =
    case model.state of
        Loaded pmodel ->
            App.map PageMsg (Pages.view pmodel)

        Loading ->
            div [] [ text "Loading..." ]

        Failed ->
            div [] [ text "Failed to load!" ]

                
subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Loaded pmodel ->
            Sub.map PageMsg (Pages.subscriptions pmodel)

        _ ->
            Sub.none
