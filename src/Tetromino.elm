module Tetromino where

import Array (Array)
import Array

type alias Location = (Int, Int)

-- A Tetromino is an Array where each entry which contains a list of 4 
-- Locations. Each entry in the array is a possible rotation of the Tetromino.
type alias Tetromino = 
    { 
      ix : Int
    , state : Array (List Location)
    }

-- A line piece 
line : Tetromino
line = 
    let state = 
                 Array.fromList 
                          [
                           -- ****
                            [ (1,0), (1,1), (1,2), (1,3) ]
                            
                          , [ (0,2) -- *
                            , (1,2) -- *
                            , (2,2) -- *
                            , (3,2) -- *
                            ]

                           -- ****
                          , [ (2,0), (2,1), (2,2), (2,3) ]

                          , [ (0,1) -- *
                            , (1,1) -- *
                            , (2,1) -- *
                            , (3,1) -- *
                            ]
                          ]
    in
       Tetromino 0 state

-- A square piece
-- **
-- **
square : Tetromino
square = 
    let state = Array.fromList 
                        [ 
                          [ 
                           (0,0), (0,1),
                           (1,0), (1,1) 
                          ]
                        ]
    in
       Tetromino 0 state
-- A Z piece
-- **
--  **
zpiece : Tetromino
zpiece = 
    let state = Array.fromList 
                        [ 
                          [ (0,0), (0,1),
                                   (1,1), (1,2)
                          
                          ]

                        , [              (0,2),
                                  (1,1), (1,2),
                                  (2,1)
                          ]
                          
                        , [
                           (1,0), (1,1), 
                                  (2,1), (2,2)
                          ]
                        , [       (0,1),
                           (1,0), (1,1),
                           (2,0)
                          ]
                        ]
    in
       Tetromino 0 state
                      

-- An S piece
--  **
-- **
spiece : Tetromino
spiece = 
    let state = Array.fromList 
                        [ 
                          [        (0,1), (0,2),
                            (1,0), (1,1)
                          ]
                          
                        , [        (0,1),
                                   (1,1), (1,2),
                                          (2,2)
                          ]

                        , [
                                   (1,1), (1,2),
                            (2,0), (2,1)
                          ] 

                        , [ (0,0),
                            (1,0), (1,1),
                                   (2,1)
                          ]
                        ]
    in
       Tetromino 0 state

-- A J piece
--  *
--  *
-- **
jpiece : Tetromino
jpiece = 
    let state = Array.fromList
                        [
                          [ (0,0),
                            (1,0), (1,1), (1,2)
                          ]

                        , [       (0,1), (0,2),
                                  (1,1),
                                  (2,1)
                          ]

                        , [
                            (1,0), (1,1), (1,2),
                                          (2,2)
                          ]

                        , [        (1,0),
                                   (1,1),
                            (0,2), (1,2)
                          ]

                          
                        ]
    in
       Tetromino 0 state

-- An L piece
-- *
-- *
-- **
lpiece : Tetromino
lpiece =
    let state = Array.fromList 
                        [ 
                          [               (0,2),
                            (1,0), (1,1), (1,2)
                          ]

                        , [        (0,1),
                                   (1,1),
                                   (2,1), (2,2)]

                        , [
                            (1,0), (1,1), (1,2),
                            (2,0)
                          ]

                        , [ (0,0),
                            (1,0),
                            (2,0), (2,1)]
                        ]
    in 
       Tetromino 0 state

-- A T piece
-- ***
--  *
tpiece : Tetromino
tpiece = 
    let state = Array.fromList
                        [ 
                          [        (0,1),
                            (1,0), (1,1), (1,2)
                          ]

                        , [        (0,1),
                                   (1,1), (1,2),
                                   (2,1)
                          ]

                        , [
                            (1,0), (1,1), (1,2),
                                   (2,1)
                          ]

                        , [        (0,1),
                            (1,0), (1,1),
                                   (2,1)
                          ]
                           
                        ]
    in
       Tetromino 0 state
                   

type Rotation 
    = CW
    | CCW

-- Given a direction to rotate, and Tetromino,
-- return a Tetromino that is the result of rotating the
-- specified Tetromino in that direction.
rotate : Rotation -> Tetromino -> Tetromino
rotate rotation tetromino = 
    let direction = case rotation of
                      CW -> 1
                      CCW -> -1
        max = Array.length tetromino.state
        ix' = (tetromino.ix + direction) % max
    in
       { tetromino | 
         ix <- ix'
       }

