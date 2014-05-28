module Tetromino where

import Location (..)
import Util

-- A Tetromino is a list of Locations. By definition, a valid tetromino
-- should contain exactly 4 different locations
type Tetromino = [Location]

-- A line piece 
-- ****
line : Tetromino
line = [(0,0), (1,0), (2,0), (3,0)]

-- A square piece
-- **
-- **
square : Tetromino
square = [(0,0), (1,0),
          (0,1), (1,1)]
-- A Z piece
-- **
--  **
zpiece : Tetromino
zpiece = [(0,0), (1,0),
                 (1,1), (2,1)]
-- An S piece
--  **
-- **
spiece : Tetromino
spiece = [       (1,0), (2,0),
          (0,1), (1,1)]

-- A J piece
--  *
--  *
-- **
jpiece : Tetromino
jpiece = [       (1,0),
                 (1,1),
          (0,2), (1,2)]

-- An L piece
-- *
-- *
-- **
lpiece : Tetromino
lpiece = [(0,0),
          (0,1),
          (0,2), (1,2)]

-- A T piece
-- ***
--  *
tpiece : Tetromino
tpiece = [(0,0), (1,0), (2,0),
                 (1,1)]