module TetrisColor where

import Color (..)

data TetrisColor = Red | Orange | Yellow | Green | Blue | Indigo | Violet | Shadow

toColor : TetrisColor -> Color
toColor c = 
  case c of
    Red -> red
    Orange -> orange
    Yellow -> yellow
    Green -> green
    Blue -> lightBlue
    Indigo -> darkBlue
    Violet -> purple
    Shadow -> rgba 255 255 255 0.10
    