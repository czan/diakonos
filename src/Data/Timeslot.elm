module Data.Timeslot exposing (Day(..), Time, Timeslot, days, times, all, table, jsonDecode)

import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Json.Decode as Json exposing ((:=))

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

all = List.concatMap (\a -> List.map (\b -> (a,b)) times) days

daytitle : Day -> Html a
daytitle day =
    strong []
        [text (case day of
                   Monday -> "mo"
                   Tuesday -> "tu"
                   Wednesday -> "we"
                   Thursday -> "th"
                   Friday -> "fr")]
             
timetitle : Time -> Html a
timetitle time =
    strong []
        [time
        |> toString
        |> String.padLeft 2 '0'
        |> String.padRight 4 '0'
        |> text]

table : (Timeslot -> Html a) -> Html a
table cellFn =
    let
        cell x = td [] [x]
                 
        titlerow =
            tr []
                (td [] [] :: List.map (cell << daytitle) days)

        row time =
            let
                process day = cellFn (day, time)
            in
                tr [] (timetitle time :: List.map (cell << process) days)
                
    in
        Html.table [class "timetable"]
            (titlerow :: List.map row times)

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

jsonDecode : Json.Decoder Timeslot
jsonDecode =
    Json.object2 (,)
        ("day" := decodeDay)
        ("time" := Json.int)
