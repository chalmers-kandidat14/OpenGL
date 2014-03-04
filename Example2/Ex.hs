{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleContexts, NoMonomorphismRestriction #-}

import qualified Disp.Data as D
import Disp
import Disp.Util

import Beads
import Camera
import Grid
import HPController

import Control.Concurrent (MVar, newEmptyMVar, newMVar)
import Graphics.UI.GLUT
import Data.Has

import qualified Data.Vector as V
import HPModel
import Chain
import Coord
import Metro
import Moves

import System.Random.MWC
import Control.Monad.Primitive

type State = FieldOf Beads :&: FieldOf Camera :&: FieldOf HPState

beads0 :: [Bead]
beads0 = [(Vector3 0 0 0, Color4 1 1 1 1)]
{-
beads0 = [ (Vector3   0    0    0 , Color4 1 1 1 1)
         , (Vector3   5    0    0 , Color4 0 1 0 1)
         , (Vector3 (-5)   0    0 , Color4 0 1 0 1)
         , (Vector3   0    5    0 , Color4 1 0 0 1)
         , (Vector3   0  (-5)   0 , Color4 1 0 0 1)
  mv <- newEmptyMVar
         , (Vector3   0    0    5 , Color4 0 0 1 1)
         , (Vector3   0    0  (-5), Color4 0 0 1 1) ]
-}

-- state0 = fieldOf beads0 & fieldOf (camera0 (0, 0, 0))

main :: IO ()
main = do
  g <- createSystemRandom
  let cbks  = [ D.Display focus
              , D.Display beadRenderer
              , D.Display $ gridRenderer (Color4 0.5 0.5 0.5 1) 50 1
              , D.Display $ cameraRenderer 0.5
              , D.Reshape cameraReshape
              , D.KeyboardMouse cameraKeyboardMouse
              , D.Motion cameraMotion 
              , D.Idle (idleHP g 1000) 0.05 ]

      wopts = D.WindowOptions
                { D.windowSize = (500, 500)
                , D.windowPos  = (100, 100)
                , D.windowName = Nothing }


  let residues :: V.Vector HPResidue
      residues = V.fromList $ createResidues "HHPPHPHPPHHHPPHPHPHPPH"
      chain :: Chain Coord2d
      chain = createChain (V.length residues)
      temps, temps' :: [Double]
      temps' = generateTemps 10000
      temps  = temps' ++ repeat (last temps')
  
  mv <- newMVar chain


  let hpState0 :: (V.Vector HPResidue, Chain Coord2d, MVar (Chain Coord2d), [Double])
      hpState0 = (residues, chain, mv, temps)

      state0 :: State
      state0 =   fieldOf beads0
               & fieldOf (camera0 (0, 0, 0))
               & fieldOf hpState0

  disp cbks state0 wopts


createChain :: Int -> Chain Coord2d
createChain n = fromList [Coord2d 1 x | x <- [1..n]]


generateTemps :: Int -> [Double]
generateTemps n = [f t | t <- [0..n]]
    where f t = (1000 * (1 - (fromIntegral t)^2 / (fromIntegral n)^2))
