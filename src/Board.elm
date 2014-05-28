module Board where

import Tetromino (Tetromino)
import Location (..)
import TetrisColor (..)
import Dict (Dict, toList, empty)
import Dict

-- Board is a dictionary of Locations and the color of the block there
type Board = Dict Location TetrisColor

boardWidth = 10
boardHeight = 2*boardWidth

emptyBoard = empty

asElement : Board -> Int -> Element
asElement b blockSize = 
  let width = blockSize*boardWidth in
  let height = blockSize*boardHeight in
  let fWidth = (toFloat width) in
  let fHeight = (toFloat height) in
  let fHeight = 2*fWidth in
  let fblockSize = toFloat blockSize in
  let background = filled black <| rect fWidth fHeight in
  let offset = (-(fWidth/2)+(fblockSize/2), (fHeight/2)-(fblockSize/2)) in
  let blocks = move offset <| Dict.foldr (accForm fblockSize) (filled white <| rect 0 0) b in
  collage width height [background, blocks]
  
insertTetromino : (Tetromino, TetrisColor) -> Board -> Board  
insertTetromino (toAdd, color) b = foldr (\loc -> insert loc color) b toAdd

insert : Location -> TetrisColor -> Board -> Board
insert = Dict.insert

accForm : Float -> Location -> TetrisColor -> Form -> Form
accForm blockSize loc color acc = group [toForm blockSize loc color, acc]

toForm : Float -> Location -> TetrisColor -> Form
toForm blockSize (x, y) color = 
  let block = filled (toColor color) (square blockSize) in
  let outline = outlined (solid black) (square blockSize) in
  let tX = (toFloat x)*blockSize in
  let tY = -(toFloat y)*blockSize in
  move (tX, tY) <| group [outline, block]