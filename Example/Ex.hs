{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleContexts, NoMonomorphismRestriction #-}

import qualified Disp.Data as D
import Disp
import Disp.Util

import Beads
import Camera
import Grid

import Control.Concurrent (threadDelay, yield)
import Graphics.UI.GLUT
import Data.Has

type State = FieldOf Beads :&: FieldOf Camera

beads0 :: [Bead]
beads0 = [ (Vector3   0    0    0 , Color4 1 1 1 1)
         , (Vector3   5    0    0 , Color4 0 1 0 1)
         , (Vector3 (-5)   0    0 , Color4 0 1 0 1)
         , (Vector3   0    5    0 , Color4 1 0 0 1)
         , (Vector3   0  (-5)   0 , Color4 1 0 0 1)
         , (Vector3   0    0    5 , Color4 0 0 1 1)
         , (Vector3   0    0  (-5), Color4 0 0 1 1) ]

state0 :: State
state0 = fieldOf beads0 & fieldOf (camera0 (0, 0, 0))

main :: IO ()
main = do
  let cbks  = [ D.Display focus
              , D.Display beadRenderer
              , D.Display $ gridRenderer (Color4 0.5 0.5 0.5 1) 20 1
              , D.Display $ cameraRenderer 0.5
              , D.Reshape cameraReshape
              , D.KeyboardMouse cameraKeyboardMouse
              , D.Motion cameraMotion]

      wopts = D.WindowOptions
                { D.windowSize = (500, 500)
                , D.windowPos  = (100, 100)
                , D.windowName = Nothing }

  disp cbks state0 wopts



