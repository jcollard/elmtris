module Tetris where

import open Util
import open Tetromino
import open TetrisColor
import open Board
import open Control
import Dict (Dict, fromList, member, findWithDefault)
import Keyboard (arrows, keysDown)
import Random (range)
import Char (toCode, fromCode)
import Graphics.Element as G

type Piece = (Tetromino, TetrisColor)

hardDropKey : Int
hardDropKey = toCode ' '

pieceDict : Dict Int Piece
pieceDict = fromList . zip [0..6] <| pieces
pieces : [Piece]
pieces = 
  zip (map (shift (4, 0)) [line, square, zpiece, spiece, jpiece, lpiece, tpiece])
      [Red,  Orange, Yellow, Green,  Blue,   Indigo, Violet]

getPiece : Int -> Piece
getPiece n = findWithDefault (head pieces) n pieceDict

game = { board=emptyBoard, 
         falling=(getPiece 0), 
         arrow=(0,0), 
         keys=[], 
         level=1, 
         score=0,
         lines=0,
         tick=0 }

score x =
  case x of
    1 -> 25
    2 -> 100
    3 -> 400
    4 -> 1000
    _ -> 0

handle (arrow, keys, t, next) = autoDrop t next . arrowControls arrow . keyControls keys

autoDrop t n game =
  let time = (inSeconds t) in
  let drop = (time - game.tick) > ((1/game.level) + 0.25) in
  let next = doControl (Just Drop) game in
  let board = game.board in
  let (tr, color) = game.falling in
  let set = checkSet (board, tr) in
  if set 
   then 
    let (newBoard, linesCleared) = clearBoard <| insertTetromino (tr, color) board in
    let nextPiece = getPiece n in
    let totalLines = (game.lines + linesCleared) in
  {game | board <- newBoard, falling <- nextPiece, score <- game.score+(score linesCleared), lines <- totalLines, level <- getLevel totalLines}
   else
    if drop then {next | tick <- time} else game

getLevel : Int -> Float
getLevel n = toFloat <| (n `div` 10) + 1

keyControls ks game = 
  let stop = ks == game.keys in
  let g = foldr doControl game (map getKeyControl ks) in
  if stop then game else {g | keys <- ks}
    
getKeyControl : Int -> Maybe Control
getKeyControl k =
   if (k == hardDropKey) then Just HardDrop else Nothing

arrowControls arr game = 
  let x = arr.x in
  let y = arr.y in
  let stop = game.arrow == (x,y) in
  let g = (flip doControl) game <| getArrowControl (x, y) in
  if stop then game else {g | arrow <- (x, y)}
      
getArrowControl : (Int, Int) -> Maybe Control         
getArrowControl arrow =  
  case arrow of
    (-1, 0) -> Just MoveLeft
    (1, 0) -> Just MoveRight
    (0, -1) -> Just Drop
    (0, 1) -> Just <| Rotate CW
    _ -> Nothing
  
doControl c game =
  let board = game.board in
  let (tr, color) = game.falling in
  case c of
   Nothing -> game
   Just c ->
      let (board', tr') = control (board, tr) c in
      {game | board <- board', falling <- (tr', color)}

label l r = flow G.right [plainText l, spacer 5 5, asText r]
  
scoreBoard game = flow down [label "Score: " game.score,
                             label "Level: " game.level,
                             label "Lines: " game.lines]
            
render game =
  let withPiece = insertTetromino (game.falling) (game.board) in
  flow G.right [asElement withPiece 300, scoreBoard game]


inputSignal = lift4 (,,,) arrows keysDown (every (second/32)) (range 0 6 (every millisecond))

main = render <~ (foldp handle game inputSignal)


--main = asText <~ (foldp handle game inputSignal)