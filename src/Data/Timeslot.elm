module Data.Timeslot exposing (Day(..), Time, Timeslot, days, times, all, table, decode, encode, toString)

import Html exposing (..)
import String
import Json.Decode as Json exposing ((:=))
import Json.Encode as E

type Day = Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
type alias Time = Int
type alias Timeslot = (Day, Time)

days : List Day
days = [Monday, Tuesday, Wednesday, Thursday, Friday]

times : List Time
times = [8 .. 16]

all : List Timeslot
all = List.concatMap (\a -> List.map (\b -> (a,b)) times) days

daytitle : Day -> Html a
daytitle day =
    td []
        [ strong []
              [ text (case day of
                          Monday -> "mo"
                          Tuesday -> "tu"
                          Wednesday -> "we"
                          Thursday -> "th"
                          Friday -> "fr")
              ]
        ]

timeString : Int -> String
timeString =
    Basics.toString
    >> String.padLeft 2 '0'
    >> String.padRight 4 '0'

toString : Timeslot -> String
toString (day, time) =
    Basics.toString day ++ "@" ++ timeString time
             
timetitle : Time -> Html a
timetitle time =
    td []
        [ strong []
              [ text <| timeString time ]
        ]

table : List (Attribute a) -> (Timeslot -> Html a) -> Html a
table attrs cellFn =
    let
        titlerow =
            tr []
                (td [] [] :: List.map daytitle days)

        row time =
            let
                title = timetitle
                process day = cellFn (day, time)
            in
                tr [] (title time :: List.map process days)
                
    in
        Html.table attrs (titlerow :: List.map row times)

decodeDay : Json.Decoder Day
decodeDay =
    Json.string `Json.andThen`
        (\x -> case String.toLower x of
                   "monday" -> Json.succeed Monday
                   "tuesday" -> Json.succeed Tuesday
                   "wednesday" -> Json.succeed Wednesday
                   "thursday" -> Json.succeed Thursday
                   "friday" -> Json.succeed Friday
                   _ -> Json.fail "Invalid day")

decode : Json.Decoder Timeslot
decode =
    Json.object2 (,)
        ("day" := decodeDay)
        ("time" := Json.int)

encode : Timeslot -> E.Value
encode (day, time) =
    E.object
        [ ("day", E.string <| Basics.toString day)
        , ("time", E.int time)
        ]
