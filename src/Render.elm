module Render where

import Board (Board, BlockStyle, Location)
import Board
import Color (..)
import Dict
import Graphics.Collage (..)
import List ((::))
import List

-- The size of each block
blockSize : Float
blockSize = 25

-- The rendered width of the board
width : Float
width = blockSize * (toFloat Board.width)

-- The rendered height of the board
height : Float
height = blockSize * (toFloat Board.height)

-- The top left corner of the board
left : Float
left = -width/2

top : Float
top = height/2

offset : (Float, Float)
offset = (left, top)


-- Given a board, return a list of forms that can be rendered to draw the board
board : Board -> List Form
board toRender =
    let background = rect width height |> filled black
        blocks = List.map block (Dict.toList toRender)
    in background :: blocks

-- Returns a Form representing a block at the specified location
block : (Location, BlockStyle) -> Form
block ((row, col), block) =
    let offy = -blockSize * (toFloat row)
        offx = blockSize * (toFloat col)
    in
        move (left + offx, top + offy) block