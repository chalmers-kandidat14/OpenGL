{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleContexts, NoMonomorphismRestriction #-}

module Beads where

import Data.Has
import Graphics.UI.GLUT
import Camera

data Beads = Beads
type instance TypeOf Beads = [Bead]

type Bead = (Vector3 GLfloat, Color4 GLfloat)


beadRenderer :: (Has Beads r, Has CameraState r) => r -> IO ()
beadRenderer r = do
  clear [ ColorBuffer, DepthBuffer ]
  let beads = Beads ^. r
  preservingMatrix $ do
    --focus r
    mapM_ renderBead beads
  where
    renderBead (pos, clr) = do
      preservingMatrix $ do
        color clr
        translate pos
        renderObject Solid (Sphere' 0.5 50 20)






