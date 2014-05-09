
module Grid where

import Graphics.UI.GLUT
import Disp.Util


gridRenderer :: (Color4 GLfloat) -> Int -> Float -> s -> IO ()
gridRenderer clr n d _ = do
  preservingMatrix $ do
    color clr
    renderPrimitive Lines $ mapM_ vertex lines
  where
    n' = fromIntegral n
    dl = n'*d / 2
    ds = scanl (\x _ -> x + d) (-dl) [0..n-1]
    lines  = linesX ++ linesY
    linesX = [ Vertex3 x y 0 | x' <- ds
                             , y' <- [-dl, dl]
                             , let x = toGLfloat x'
                             , let y = toGLfloat y' ] 
    linesY = [ Vertex3 x y 0 | y' <- ds
                             , x' <- [-dl, dl]
                             , let x = toGLfloat x'
                             , let y = toGLfloat y' ] 



