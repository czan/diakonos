module DragAndDrop exposing (DragType, dragData, draggable, onDrop, dropTarget)

import Html
import Html.Attributes exposing (property)
import Html.Events exposing (onWithOptions)
import Json.Encode as JsonE
import Json.Decode as JsonD
import Native.DragAndDrop


type alias DragType a =
    { key : String
    , encode : a -> JsonE.Value
    , decode : JsonD.Decoder a
    }


setData : String -> JsonE.Value -> JsonD.Decoder a
setData = Native.DragAndDrop.setData


dragData : DragType a -> a -> Html.Attribute msg
dragData dt value =
    let options = { preventDefault = True
                  , stopPropagation = True
                  }
    in onWithOptions "dragstart" options (setData dt.key (dt.encode value))


draggable : Bool -> Html.Attribute a
draggable = property "draggable" << JsonE.bool


decodeData : String -> JsonD.Decoder JsonD.Value
decodeData = Native.DragAndDrop.decodeData


onDrop : DragType a -> (a -> msg) -> Html.Attribute msg
onDrop dt tagger =
    let options = { preventDefault = True
                  , stopPropagation = True
                  }
        decodeValue value = JsonD.decodeValue dt.decode value
        decoder = decodeData dt.key `JsonD.customDecoder` decodeValue
    in onWithOptions "drop" options (JsonD.map tagger decoder)


dropTarget : Bool -> Html.Attribute a
dropTarget okay = if okay then
                      Native.DragAndDrop.dropTarget
                  else
                      property "dropTarget" (JsonE.bool False)
