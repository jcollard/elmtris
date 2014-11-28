module State where

import Array
import Board (Board, Location)
import Board
import Dict
import Maybe (Maybe, (?))
import List
import Random
import Tetromino (Tetromino)


type alias State = 
    { 
      -- The current Board
      board : Board
      -- The top left of the current Tetromino
    , loc : Location
      -- The current Tetromino
    , tetromino : Tetromino
      -- Upcoming Tetrominos
    , next : List Tetromino
      -- Nothing if no Tetromino is being held
    , hold : Maybe Tetromino
      -- Randomness
    , seed : Maybe Random.Seed
    }

noSeed : Random.Seed
noSeed = Random.initialSeed 42

bounds : List (Int, Int) -> ((Int, Int), (Int, Int))
bounds blocks = 
    let (rs, cs) = List.unzip blocks 
    in
        ((List.minimum rs, List.minimum cs), (List.maximum rs, List.maximum cs))

-- A State is valid if the current piece is within the bounds of the screen and
-- it is not overlapping any existing blocksg
valid : State -> Bool
valid { board, loc, tetromino } =
    let (offR, offC) = loc
        blocks = List.map (\(r,c) -> (offR+r,offC+c)) <| (Array.get tetromino.ix tetromino.state) ? []
        ((minRow, minCol), (maxRow, maxCol)) = bounds blocks
        inBounds = minRow >= -2 && 
                   minCol >= 0 &&
                   maxRow < Board.height &&
                   maxCol < Board.width
        noCollisions = List.foldr (\loc acc -> (not (Dict.member loc board)) && acc) True blocks
    in noCollisions && inBounds