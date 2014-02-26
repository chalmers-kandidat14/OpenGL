

import qualified Data as D
import Setup

import Graphics.UI.GLUT

type State = ()

disp :: State -> IO State
disp _ = do
  clear [ ColorBuffer, DepthBuffer ]
  color (Color4 0.5 0.5 0.5 1 :: Color4 GLdouble)
  renderObject Solid (Sphere' 1 40 20)
  flush
  swapBuffers

rshp :: State -> Size -> IO State
rshp _ size@(Size w h) = do
  viewport $= (Position 0 0, size)
  matrixMode $= Projection
  loadIdentity
  let wf = fromIntegral w
  let hf = fromIntegral h
  if w <= h
    then ortho (-1.5) 1.5 (-1.5 * hf / wf) (1.5 * hf / wf) (-10) 10
    else ortho (-1.5 * wf / hf) (1.5 * wf / hf) (-1.5) 1.5 (-10) 10
  matrixMode $= Modelview 0
  loadIdentity

main :: IO ()
main = do
  let cbks  = (D.emptyCallbacks ())
               { D.display = disp
               , D.reshape = rshp }

      wopts = D.WindowOptions
                { D.windowSize = (500, 500)
                , D.windowPos  = (100, 100)
                , D.windowName = Nothing }

      opts  = D.Options
                { D.callbacks = cbks
                , D.windowOptions = wopts }

  setup opts
