module Myaudio (sayWord) where

import qualified SDL
import SDL.Mixer 

sayWord :: String -> IO ()
sayWord wd = do
  let fileName = "./audio/"++wd++".mp3"
  SDL.initialize [SDL.InitAudio]
  withAudio defaultAudio 4096 $ do
    load fileName >>= play
    SDL.delay 1000 
  SDL.quit
