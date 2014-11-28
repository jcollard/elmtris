module Control where

import Tetromino (Rotation)
import Tetromino

import State (State)
import State

type Control 
    = Left
    | Right
    | Down
    | Drop
    | Rotate Rotation

update : Control -> State -> State
update control state =
    let state' = handle control state
    in
        if  | State.valid state' -> state'
            | otherwise -> state

handle : Control -> State -> State
handle control state =
    case control of
      Left -> shift (0, -1) state
      Right -> shift (0, 1) state
      Down -> shift (1, 0) state
      Drop -> drop state
      Rotate dir -> rotate dir state

rotate : Rotation -> State -> State
rotate rotation state =
    let tetromino' = Tetromino.rotate rotation state.tetromino
    in 
        { state | 
          tetromino <- tetromino'
        }

drop : State -> State
drop state =
    let next = handle Down state
    in
        if  | (State.valid next) -> drop next
            | otherwise -> state 

shift : (Int, Int) -> State -> State
shift (r, c) state =
    let (currR, currC) = state.loc
    in
        { state |
          loc <- (currR+ r, currC + c)
        }