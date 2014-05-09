{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleContexts, NoMonomorphismRestriction #-}

import System.Environment
import qualified Data.Char as C

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

main :: IO ()
main = do
  args <- getArgs
  cont <- getContents >>= return . words
 
  let xs = args ++ cont
  let cs = coords xs
  let rs = residues xs
  let bs = zipWith toBead cs rs

  putStrLn $ "coords: " ++ show cs
  putStrLn $ "residues" ++ show rs

  let state0 = fieldOf bs & fieldOf (camera0 (0, 0, 0)) :: State

  let cbks  = [ D.Display focus
              , D.Display beadRenderer
              , D.Display $ gridRenderer (Color4 0.7 0.7 0.7 1) 100 1
              , D.Display $ cameraRenderer 0.5
              , D.Reshape cameraReshape
              , D.KeyboardMouse cameraKeyboardMouse
              , D.Motion cameraMotion]

      wopts = D.WindowOptions
                { D.windowSize = (500, 500)
                , D.windowPos  = (100, 100)
                , D.windowName = Just "hpview"}

  disp cbks state0 wopts


residues = filter isHPN . map (map C.toLower)
      where
        isHPN "h" = True
        isHPN "p" = True
        isHPN "n" = True
        isHPN _   = False

coords :: [String] -> [(GLfloat, GLfloat, GLfloat)]
coords = f . map read . filter (C.isDigit . head)
      where
        f (x:y:z:xs) = (x,y,z) : f xs
        f _          = []


toBead (x,y,z) r = (Vector3 (-x) (-y) (-z), c)
  where
    c = case r of
          "h" -> Color4 1 0 0 1
          "n" -> Color4 0 1 0 1
          "p" -> Color4 0 0 1 1
      




