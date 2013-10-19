module Control where

import open Location
import open Tetromino
import open Board
import Dict (member)

data Control = MoveLeft
             | MoveRight
             | Drop
             | HardDrop
             | Rotate Rotation
               
type GameState = (Board, Tetromino)
               
             

-- A bound is a pair of minimum and maximum locations. Typically,
-- A bound is the containing box of a tetromino
type Bound = (Location, Location)

-- A Tetromino can be rotate Clockwise or Counter Clockwise
data Rotation = CW
              | CCW
                
left : Tetromino -> Tetromino
left = shift (-1, 0)

right : Tetromino -> Tetromino
right = shift (1, 0)

drop : Tetromino -> Tetromino
drop = shift (0, 1)

hardDrop : GameState -> GameState
hardDrop g = 
 let next = forceControl g Drop in
 if isValidState next then hardDrop next else g


shift : (Int, Int) -> Tetromino -> Tetromino
shift (offX, offY) tr = 
  map (\(x0, y0) -> (x0+offX, y0+offY)) tr
  
-- Given a direction to rotate, and Tetromino,
-- return a Tetromino that is the result of rotating the
-- specified Tetromino in that direction.
rotate : Rotation -> Tetromino -> Tetromino
rotate rot tr = 
  case rot of
    CCW -> rotate CW . rotate CW . rotate CW <| tr
    CW ->
      let ((minX, minY), (maxX, maxY)) = bounds tr in
      let rows = maxY - minY in
      let cols = maxX - minX in
      let rt (c, r) = ((rows)-r, c) in
      map rt tr

-- Given a Tetromino, return the bounding box that encompasses
-- all of its locations
bounds : Tetromino -> Bound
bounds tr = 
  let (xs, ys) = unzip tr in
  ( (minimum xs, minimum ys), (maximum xs, maximum ys))

isValidState : GameState -> Bool
isValidState (board, tr) = 
  let noCollision = foldr (\loc acc -> (not (member loc board)) && acc) True tr in
  let ((minX, minY), (maxX, maxY)) = bounds tr in
  let inBounds = minX >= 0 && minY >= 0 && maxX <= boardWidth && maxY <= boardHeight in
  noCollision && inBounds
  
  
  
control : GameState -> Control -> GameState
control s c = 
  let forced = forceControl s c in
  if (isValidState forced) then forced else s
  
forceControl : GameState -> Control -> GameState
forceControl (board, tr) control =
 case control of
   MoveLeft -> (board, left tr)
   MoveRight -> (board, right tr)
   Drop -> (board, drop tr)
   HardDrop -> hardDrop (board, tr)
   Rotate r -> (board, rotate r tr)