
module Data where

import Prelude hiding ( init )
import Graphics.UI.GLUT

type KeyboardMouse = ( Key        -- Indicates which key or button is pressed or released
                     , KeyState   -- Indicates whether the key is pressed or released
                     , Modifiers  -- Indicates whether the ctrl or alt key is pressed
                     , Position ) -- Mouse pointer position

data MotionType = Active | Passive
  deriving (Show, Read, Eq)

data Motion =
  Motion { motionType :: MotionType
         , position   :: Position
         } deriving (Show, Eq)

data Callbacks s =
  Callbacks { init          :: IO s
            , display       :: s -> IO s 
            , reshape       :: s -> Size -> IO s
            , keyboardMouse :: s -> KeyboardMouse -> IO s
            , motion        :: s -> Motion        -> IO s
            , idle          :: Maybe (s -> IO s)
            }

emptyCallbacks :: s -> Callbacks s
emptyCallbacks s =
  Callbacks { init          = return s
            , display       = return
            , reshape       = \a _ -> return a
            , keyboardMouse = \a _ -> return a
            , motion        = \a _ -> return a
            , idle          = Nothing
            }

data WindowOptions = 
  WindowOptions { windowSize :: (Int, Int)
                , windowPos  :: (Int, Int)
                , windowName :: Maybe String
                } deriving (Show, Eq)

data Options s =
  Options { callbacks :: Callbacks s
          , windowOptions :: WindowOptions
          }



