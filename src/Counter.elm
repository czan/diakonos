module Counter exposing ( Model, Msg, init, update, view )
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

type alias Model = Int

type Msg = Increment | Decrement


init : Int -> Model
init x = x


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (toString model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
