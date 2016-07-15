module Main exposing (main)

import Html.App as App
import Pages
import Dict exposing (Dict)
import Data.Person as Person exposing (Person)
import Data.Group as Group exposing (Group)
import Html exposing (..)
import Json.Decode as Json exposing ((:=), Decoder)
import Json.Encode as E
import Task
import Http


type Msg
    = RestoreData (Person.Dict, Group.Dict)
    | RestoreFail Http.Error
    | PageMsg Pages.Msg
    | SaveFail Http.Error
    | Saved (Person.Dict, Group.Dict)


type State
    = Loaded Pages.Model
    | Loading
    | Failed


type alias Model =
    { people : Person.Dict
    , groups : Group.Dict
    , url : String
    , state : State
    }


main : Program Never
main =
    App.program
        { init = init "./data.json"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


requestDecoder : Decoder (Person.Dict, Group.Dict)
requestDecoder =
    Json.object2 (,)
        ("people" := Json.dict Person.decode)
        ("groups" := Json.dict Group.decode)


init : String -> (Model, Cmd Msg)
init url =
    let request = Http.get requestDecoder url
    in { people = Dict.empty
       , groups = Dict.empty
       , url = url
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
               , url = model.url
               , state = Loaded pmodel
               } ! [ Cmd.map PageMsg pcmd ]

        (_, Loaded _) ->
            model ! []

        (_, Loading) ->
            model ! []

        (_, Failed) ->
            model ! []


encode : Person.Dict -> Group.Dict -> E.Value
encode people groups =
    let people' = Dict.map (always Person.encode) people
                  |> Dict.toList
                  |> E.object
        groups' = Dict.map (always Group.encode) groups
                |> Dict.toList
                |> E.object
    in E.object [ ("people", people')
                , ("groups", groups')
                ]
            

updateFromPages : Pages.ParentMsg -> Model -> (Model, Cmd Msg)
updateFromPages msg model =
    case msg of
        Pages.SaveData people groups ->
            let request = Http.post requestDecoder model.url (Http.string "bleepblah")
                model' = { model | people = people, groups = groups }
            in
                model' ! [ Task.perform SaveFail Saved request ]

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
