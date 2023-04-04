{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL.Init (initializeAll)
import qualified SDL.Font as F
import SDL.Video (createWindow,defaultWindow,windowInitialSize,destroyWindow
                 ,createRenderer,defaultRenderer,Renderer,Texture)
import SDL.Event (EventPayload(KeyboardEvent),pollEvents,eventPayload,keyboardEventKeyMotion
                 ,InputMotion(Pressed),keyboardEventKeysym)
import SDL.Video.Renderer (loadBMP,createTextureFromSurface,rendererDrawColor,clear,drawPoint
                          ,drawLine,copy,present,Rectangle(..))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import SDL.Vect (Point(P),V2(..),V4(..))
import SDL.Input.Keyboard (Keysym(keysymKeycode))
import SDL.Input.Keyboard.Codes
import SDL (($=))
import Foreign.C.Types (CInt)
import Data.List (elemIndex)

main :: IO ()
main = do
  initializeAll
  let myWindow = defaultWindow {windowInitialSize =  V2 480 600}
  F.initialize
  font <- F.load "font/monaco.ttf" 24
  fontS <- F.blended font (V4 255 255 255 255) "abcdefghijklmnopqrstuvwxyz" 
  F.free font
  window <- createWindow "SDLTEST" myWindow
  image <- loadBMP "images/cook3.bmp"
  renderer <- createRenderer window (-1) defaultRenderer
  texs <- mapM (createTextureFromSurface renderer) [fontS,image]
  appLoop renderer texs 
  destroyWindow window

appLoop :: Renderer -> [Texture] -> IO ()
appLoop renderer texs = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 182 100 255 255
  clear renderer
  rendererDrawColor renderer $= V4 0 0 0 255
  drawPoint renderer (P (V2 100 100))
  drawLine renderer (P (V2 50 50)) (P (V2 80 80))
  let rects = map (\(V4 x y w h) -> Rectangle (P (V2 x y)) (V2 w h))
                                    [V4 30 300 300 50,V4 200 200 50 50]
  mapM_ (\(s,tex) -> copy renderer tex Nothing (Just s)) (zip rects texs)
  let Just ht = viaNonEmpty head texs
  showOneChar renderer ht 40 (V2 50 400) 'f'
  present renderer
  unless qPressed (appLoop renderer texs)

showOneChar :: MonadIO m => Renderer -> Texture -> CInt -> V2 CInt -> Char -> m ()
showOneChar r t s p ch =
  let (Just index) = elemIndex ch ['a'..'z']
   in copy r t (Just (Rectangle (P (V2 (fromIntegral index*14) 0)) (V2 14 24)))
               (Just (Rectangle (P p) (V2 s s)))
