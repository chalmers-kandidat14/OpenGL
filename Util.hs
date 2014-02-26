
module Util where

import Unsafe.Coerce (unsafeCoerce)
import Graphics.UI.GLUT


toGLfloat :: Float -> GLfloat
toGLfloat = unsafeCoerce

toGLint :: Int -> GLint
toGLint = unsafeCoerce

toSize :: (Int, Int) -> Size
toSize (x, y) = Size (toGLint x) (toGLint y)

toPosition :: (Int, Int) -> Position
toPosition (x, y) = Position (toGLint x) (toGLint y)




