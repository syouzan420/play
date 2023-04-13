import Data.IORef
import Graphics.UI.GLUT
import Data.List
import System.Exit
import Ether

main :: IO () 
main = do
  (_progName, args) <- getArgsAndInitialize
  (cn,rn) <- if null args 
                   then return (3::GLsizei,3::GLsizei)
                   else return (read$head args ::GLsizei,
                                read$head$tail args ::GLsizei)
  ti <- if length args==3
           then return ((read$last args) :: Timeout)
           else return (1 :: Timeout)
  tx2 <- if length args>3
           then return (unwords$drop 3 args)
           else return "0 0 10 0"
  let tx = show cn++" "++ show rn 
  let dispX = 80+24*cn
  let dispY = 70+24*rn
  initialDisplayMode $= [RGBAMode,DoubleBuffered]
  initialWindowSize $= Size dispX dispY 
  _window <- createWindow "ETHER"
  ether <- newIORef (etherInit tx tx2)
  displayCallback $= display ether dispX dispY
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardProc
  addTimerCallback ti $ timerProc (idle ether) ti
  mainLoop

etherInit :: String -> String -> Ether 
etherInit tx tx2 = 
  let cn = read$head$words tx :: Int
      rn = read$head$tail$words tx :: Int
      lst = makeList (words tx2) 
  in let bls = initBallXY cn rn firstBall
         spx = initSpringXS (cn-1) rn firstSpringX
         spy = initSpringYS cn (rn-1) firstSpringY
     in let ibl = setBallXY lst bls
            ispx = setLengthS$setSpringXS (zip ibl spx)
            ispy = setLengthS$setSpringYS (zip (transpose ibl) spy)
        in (ibl,ispx,ispy)
  
idle :: IORef Ether -> IdleCallback
idle ether = do
  eth <- get ether
  let eth' = newEther eth
  ether $= eth'
  postRedisplay Nothing

display :: IORef Ether -> GLsizei -> GLsizei -> DisplayCallback
display ether dispX dispY = do
  clearColor $= Color4 0 0 0 0
  clear [ColorBuffer]
  loadIdentity
  ortho2D 0.0 (fromIntegral dispX) 0.0 (fromIntegral dispY) 
  (bl,spx,spy) <- get ether
  currentColor $= Color4 1 0.8 0.5 0
  mapM_ (mapM_ (\b -> preservingMatrix $ do
                  translate (Vector3 (bx b+50) (by b+50) 0)
                  renderPrimitive Polygon $ mapM_ vertex [
                    Vertex3 (4*cos h) (4*sin h) 0 :: Vertex3 GLfloat | h <- [0,0.1..2*pi]
                    ])) bl
  preservingMatrix $ do
    translate (Vector3 (50::GLfloat) 50 0)
    renderPrimitive LineLoop $
      mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) 
        [(-24::GLfloat,-24::GLfloat,0::GLfloat),
        (fromIntegral dispX-80,-24,0),
        (fromIntegral dispX-80,fromIntegral dispY-70,0),
        (-24,fromIntegral dispY-70,0)]
  swapBuffers

keyboardProc :: KeyboardMouseCallback
keyboardProc ch _ _ _
  | ch == Char 'q'  = exitSuccess
  | otherwise       = return ()

timerProc :: IO () -> Timeout -> TimerCallback
timerProc act ti = do
  act
  addTimerCallback ti $ timerProc act ti

reshape :: ReshapeCallback
reshape (Size w h) = do
  viewport $= (Position 0 0 , Size w h)
  matrixMode $= Projection
  loadIdentity
  matrixMode $= Modelview 0
