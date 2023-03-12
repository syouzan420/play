{-# LANGUAGE OverloadedStrings #-}
module Main where

--import Lib
import SDL
import qualified SDL.Font as F
import Control.Monad (unless)

main :: IO ()
main = do
  initializeAll
  let myWindow = defaultWindow {windowInitialSize =  V2 480 600}
  F.initialize
  font <- F.load "font/monaco.ttf" 23
  fontS <- F.blended font (V4 255 255 255 255) "HELLO!"
  F.free font
  window <- createWindow "FuncPong" myWindow
  image <- loadBMP "images/kagunomi.bmp"
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
  rendererDrawColor renderer $= V4 100 100 100 255
  let rects = map (\(x,y,w,h) -> Rectangle (P (V2 x y)) (V2 w h))
                                    [(30,300,100,50),(200,200,100,100)]
  mapM_ (\(s,tex) -> copy renderer tex Nothing (Just s)) (zip rects texs)
  present renderer
  unless qPressed (appLoop renderer texs)
