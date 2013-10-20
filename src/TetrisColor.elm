module TetrisColor where

import open Color

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
    Shadow -> rgba 125 125 125 0.5
    