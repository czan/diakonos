module Main exposing (main)

import Html.App as App
import Pages
import Dict exposing (Dict)

main : Program Never
main =
    App.program
        { init = Pages.init Dict.empty Dict.empty
        , view = Pages.view
        , update = Pages.update
        , subscriptions = Pages.subscriptions
        }
