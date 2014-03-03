
module Disp.Util where

import Unsafe.Coerce (unsafeCoerce)
import Graphics.UI.GLUT


toGLfloat :: Float -> GLfloat
toGLfloat = unsafeCoerce

toGLint :: Int -> GLint
toGLint = unsafeCoerce

fromGLfloat :: GLfloat -> Float
fromGLfloat = unsafeCoerce

fromGLint :: GLint -> Int
fromGLint = unsafeCoerce

toSize :: (Int, Int) -> Size
toSize (x, y) = Size (toGLint x) (toGLint y)

fromSize :: Size -> (Int, Int)
fromSize (Size x y) = (fromGLint x, fromGLint y)

toPosition :: (Int, Int) -> Position
toPosition (x, y) = Position (toGLint x) (toGLint y)

fromPosition :: Position -> (Int, Int)
fromPosition (Position x y) = (fromGLint x, fromGLint y)



