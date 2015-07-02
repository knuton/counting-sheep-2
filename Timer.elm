module Timer where

import Native.Timer
import Time exposing (Time)
import Task exposing (Task)

type alias Error = { message : String }

start : String -> Time -> Time -> Task Error ()
start id duration interval = Native.Timer.start id duration interval

stop : String -> Task Error ()
stop id = Native.Timer.stop id

heat : Task a ()
heat = Native.Timer.heat

alert : String -> Task Error ()
alert msg = Native.Timer.alert msg
