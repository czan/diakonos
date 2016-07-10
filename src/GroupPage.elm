module GroupPage exposing (Model, Msg, ParentMsg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (Dict)
import Data.Group as Group exposing (Group)
import Data.Person as Person exposing (Person)
import Data.Timeslot as Timeslot exposing (Timeslot, Day(..))
import Util exposing ((!!))

type alias Model =
    { groups : Dict Group.Id Group
    , people : Dict Person.Id Person
    }
type alias Msg = Never
type ParentMsg = None

init : Dict Person.Id Person -> Dict Group.Id Group -> (Model, Cmd Msg)
init people groups =
    { groups = groups
    , people = people
    }! []

update : Msg -> Model -> (Model, Cmd Msg, ParentMsg)
update msg model =
    model ! [] !! None

view : Model -> Html Msg
view model =
    Timeslot.table (viewTimeslot model)

color : Model -> Timeslot -> String
color model timeslot =
    "#f00"

viewTimeslot : Model -> Timeslot -> Html Msg
viewTimeslot model timeslot =
    div [ class "blah"
        , style [ ("background-color", color model timeslot)
                , ("height", "30px")
                , ("width", "120px")
                ]
        ]
        [ text (toString timeslot) ]            

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
