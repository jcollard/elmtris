module Control where

import open Util
import open Location
import open Tetromino
import open Board
import Dict (empty, insert, member, toList, remove, fromList)

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
      let (rC, cC) = centerOfMass tr in
      let trans = shift (-cC, -rC) tr in
      --Stupid fix for squares and lines
      let off = if rows == cols || rows == 3 then -1 else 0 in
      let rt (c, r) = (-r, c+off) in
      let rotated = map rt trans in
      shift (cC, rC) rotated

dimensions : Tetromino -> (Int, Int)
dimensions tr =
  let ((minX, minY), (maxX, maxY)) = bounds tr in
  (maxY - minY, maxX - minX)
  
round' x =  
  let fx = floor x in
  if (x - toFloat fx) > 0.50 then fx+1 else fx
  
centerOfMass : Tetromino -> Location
centerOfMass tr =
  let (rows, cols) = dimensions tr in
  let (sumX, sumY, tot) = foldr (\(x0,y0) (x1,y1,t) -> ((toFloat x0)+x1, (toFloat y0)+y1, t+1)) (0,0,0) tr in
  (round <| sumY / tot, round' <| sumX / tot)

-- Given a Tetromino, return the bounding box that encompasses
-- all of its locations
bounds : Tetromino -> Bound
bounds tr = 
  let (xs, ys) = unzip tr in
  ( (minimum xs, minimum ys), (maximum xs, maximum ys))

clearBoard : Board -> (Board, Int)
clearBoard b = 
  let cleared = map (checkLine b) (reverse [0..19]) in
  let newBoard = clear 19 cleared b in
  let linesCleared = length . filter (\x -> x) <| cleared in
  (newBoard, linesCleared)
  
clear : Int -> [Bool] -> Board -> Board
clear n xs b =
  case xs of
    [] -> b
    (x::bs) -> 
      case x of 
        False -> clear (n-1) bs b
        True ->
          let toDrop = filter (\((_,y),_) -> y < n) . toList <| b in
          let keep = filter (\((_,y),_) -> y > n) . toList <| b in
          let drop ((x, y), color) = ((x, y+1), color) in
          let dropped = map drop toDrop in
          let cleared = fromList (dropped ++ keep) in
          clear n bs cleared

checkLine : Board -> Int -> Bool
checkLine b n =
  let locs = zip [0..9] (replicate 10 n) in
  let check loc acc = (member loc b) && acc in
  foldr check True locs

isValidState : GameState -> Bool
isValidState (board, tr) = 
  let noCollision = foldr (\loc acc -> (not (member loc board)) && acc) True tr in
  let ((minX, minY), (maxX, maxY)) = bounds tr in
  let inBounds = minX >= 0 && minY >= -2 && maxX < boardWidth && maxY < boardHeight in
  noCollision && inBounds
  
checkSet : GameState -> Bool
checkSet = not . isValidState . (flip forceControl Drop)
  
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
   Rotate r -> coerceRotate (board, rotate r tr)
   
coerceRotate : GameState -> GameState
coerceRotate state =
  case isValidState state of
    True -> state
    False -> 
      let (board, tr) = state in
      let tryLeft = (board, left tr) in
      let tryRight = (board, right tr) in
      if isValidState tryLeft then tryLeft 
        else tryRight