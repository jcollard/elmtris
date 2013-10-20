import Keyboard (arrows, keysDown)
import Time

game = { score=0 }

handle keys = clean . (handleKeys keys)

handleKeys k game = 
  {game | score <- game.score + 1}

clean game = game

render game =
  asText game
  
inputSignal = keysDown
  
main = render <~ (foldp handle game inputSignal)