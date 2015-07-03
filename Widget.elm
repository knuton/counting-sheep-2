module Widget where

import Html exposing (..)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, on, targetValue)
import Maybe
import Result
import String
import Time exposing (Time, hour, minute, second)
import Timer exposing (start, stop)
import Task exposing (Task, andThen)

type alias Model =
  { remaining : Time
  }

type Action = NoOp
            | Start
            | Set Time
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
    Start ->
      (model, StartWith model.remaining)
    -- TODO Check whether we are running
    Set time ->
      ({ model | remaining <- time }, None)
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

hours = Signal.mailbox 0
minutes = Signal.mailbox 0
seconds = Signal.mailbox 0

timeset = Signal.map3 (\x y z -> Set (x + y + z)) hours.signal minutes.signal seconds.signal

strToFloat str =
  Maybe.withDefault 0 (Result.toMaybe (String.toFloat str))

displayTime : Time -> Html
displayTime time =
  form []
    [ input
        [ on "change" targetValue (Signal.message hours.address << (\t -> t * hour) << strToFloat)
        , value (formatHours time)
        ]
        []
    , input
        [ on "change" targetValue (Signal.message minutes.address << (\t -> t * minute) << strToFloat)
        , value (formatMinutes time)
        ]
        []
    , input
        [ on "change" targetValue (Signal.message seconds.address << (\t -> t * minute) << strToFloat)
        , value (formatSeconds time)
        ]
        []
    ]

controls = Signal.mailbox NoOp

inputs = Signal.mergeMany
  [ controls.signal
  , (Signal.map Tick remaining)
  , timeset
  ]

view : Model -> Html
view model =
  div []
    [ span [] [ displayTime model.remaining ]
    , button [ onClick controls.address Start ] [ text "Start" ]
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
