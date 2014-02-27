

import qualified Data as D
import Setup
import Util

import Control.Concurrent (threadDelay, yield)
import Graphics.UI.GLUT

type State = [Bead]

disp beads = do
  clear [ ColorBuffer, DepthBuffer ]
  mapM_ displayBead beads
  return beads

rshp :: State -> Size -> IO State
rshp x size@(Size w h) = do
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
  return x

myBeads = [Bead (0, 0) H, Bead (1,0) P, Bead (1, 1) P]

test [] = yield >> return []
test s = do
  threadDelay $ 3 * 1000000
  return (tail s)


main :: IO ()
main = do
  let cbks  = (D.emptyCallbacks myBeads)
                { D.display = disp
                , D.reshape = rshp
                , D.idle    = Just test}

      wopts = D.WindowOptions
                { D.windowSize = (500, 500)
                , D.windowPos  = (100, 100)
                , D.windowName = Nothing }

      opts  = D.Options
                { D.callbacks = cbks
                , D.windowOptions = wopts }

  setup opts



data Residue = H | P
  deriving (Show, Eq)

data Bead = Bead (Int, Int) Residue
  deriving (Show, Eq)

displayBead :: Bead -> IO ()
displayBead (Bead (x, y) r) = do
  preservingMatrix $ do
    color clr
    translate pos
    renderObject Solid (Sphere' 0.5 40 20)
    
  where
    pos = Vector3 (toGLfloat . fromIntegral $ x) (toGLfloat . fromIntegral $ y) 0 
    clr = case r of
            H -> Color4 1 0 0 1 :: Color4 GLfloat
            P -> Color4 1 1 1 1 :: Color4 GLfloat





