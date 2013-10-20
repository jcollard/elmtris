import Char (toCode, fromCode)
import Keyboard (keysDown)

dropKey : Int
dropKey = toCode 'd'

getControl : [Int] -> Int -> Int
getControl k c = 
  case k of
    [] -> c
    (key::_) ->
      case key of
        dropKey -> c + 1
        _ -> c
        
main = asText <~ (foldp getControl 0 keysDown)
