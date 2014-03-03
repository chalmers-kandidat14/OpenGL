
module Data where

import Prelude hiding ( init )
import Graphics.UI.GLUT ( Key, KeyState, Modifiers, Position )

type KM = ( Key        -- Indicates which key or button is pressed or released
          , KeyState   -- Indicates whether the key is pressed or released
          , Modifiers  -- Indicates whether the ctrl or alt key is pressed
          , Position ) -- Mouse pointer position

type InitCallback s           = s -> IO s
type DisplayCallback s        = s -> IO ()
type ReshapeCallback s        = s -> (Int, Int) -> IO s
type KeyboardMouseCallback s  = s -> KM -> IO s
type MotionCallback s         = s -> (MotionType, (Int, Int)) -> IO s
type IdleCallback s           = s -> IO s

data MotionType = Active | Passive


data Callback s = Init (InitCallback s)
                | Display (DisplayCallback s)
                | Reshape (ReshapeCallback s)
                | KeyboardMouse (KeyboardMouseCallback s)
                | Motion (MotionCallback s)
                | Idle (IdleCallback s) Double

data WindowOptions = 
  WindowOptions { windowSize :: (Int, Int)
                , windowPos  :: (Int, Int)
                , windowName :: Maybe String
                } deriving (Show, Eq)



