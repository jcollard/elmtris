module Tetris where

import open Tetromino
import open TetrisColor
import open Board
import Dict (Dict, fromList, member)

type Piece = (Tetromino, TetrisColor)

pieceDict : Dict Int Piece
pieceDict = fromList . zip [0..6] <| pieces

pieces : [Piece]
pieces = 
  zip [line, square, zpiece, spiece, jpiece, lpiece, tpiece]
      [Red,  Orange, Yellow, Green,  Blue,   Indigo, Violet]

game = { board=emptyBoard, falling=(head pieces) }
