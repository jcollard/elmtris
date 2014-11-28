module Board where

import Array

import Dict (Dict)
import Dict

import Graphics.Collage (Form)
import List
import Maybe ((?))
import Tetromino (Tetromino)

type alias Location = (Int, Int)

type alias BlockStyle = Form

type alias Board = Dict Location BlockStyle

width : Int
width = 10

height : Int
height = 20

insert : (Int, Int) -> (Tetromino, BlockStyle) -> Board -> Board
insert (row, col) (toAdd, block) board = 
    let blocks = (Array.get toAdd.rotation toAdd.rotations) ? []
    in List.foldr (\(offR, offC) board' -> Dict.insert (row+offR, col+offC) block board') board blocks
