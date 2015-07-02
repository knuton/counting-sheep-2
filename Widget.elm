module Widget where

import Html exposing (..)
import Html.Events exposing (onClick)
import Time exposing (Time, hour, minute, second)
import Timer exposing (start, stop)
import Task exposing (Task, andThen)

type alias Model =
  { remaining : Time
  }

type Action = NoOp
            | Start Time
            | Tick Time
            | Halt
            | Reset

type Effect = None
            | StartWith Time
            | Stop
            | Shutdown

update : Action -> Model -> (Model, Effect)
update action model =
  case action of
    NoOp ->
      (model, None)
    Start time ->
      ({ model | remaining <- time }, StartWith time)
    Tick time ->
      ({ model | remaining <- time }, if time < 1 then Shutdown else None)
    Halt ->
      (model, Stop)
    Reset ->
      ({ model | remaining <- 0 }, Stop)

effects = Signal.map snd state

tasks =
  let p = \e ->
    case e of
      None -> Timer.heat
      StartWith time -> start "remaining" time second
      Stop -> stop "remaining"
      Shutdown -> stop "remaining" `andThen` (\x -> Timer.alert "down")
  in Signal.map p effects

port remaining : Signal Time

port effector : Signal (Task Timer.Error ())
port effector = tasks

formatTime : (Time -> Float) -> Time -> Time -> String
formatTime converter mod time =
  let modTime =
    truncate (converter (toFloat (truncate time % truncate mod)))
  in if modTime < 10 then
        "0" ++ toString modTime
      else
        toString modTime

formatSeconds : Time -> String
formatSeconds = formatTime Time.inSeconds minute

formatMinutes : Time -> String
formatMinutes = formatTime Time.inMinutes hour

formatHours : Time -> String
formatHours = formatTime Time.inHours (hour * 99)

displayTime : Time -> String
displayTime time =
  formatHours time ++ ":" ++ formatMinutes time ++ ":" ++ formatSeconds time

controls = Signal.mailbox NoOp

inputs = Signal.merge
  controls.signal
  (Signal.map Tick remaining)

view : Model -> Html
view model =
  div []
    [ span [] [ text (displayTime model.remaining) ]
    , button [ onClick controls.address (Start (7 * second)) ] [ text "Start" ]
    , button [ onClick controls.address Halt ] [ text "Halt" ]
    , button [ onClick controls.address Reset ] [ text "Reset" ]
    ]

state = Signal.foldp
  (\a me -> update a (fst me))
  ({ remaining = 0 }, None)
  inputs

main : Signal Html
main = Signal.map
  view
  (Signal.map fst state)
