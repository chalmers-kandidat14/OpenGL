
module Setup where


import Prelude hiding ( init )
import System.Exit ( exitSuccess )
import Graphics.UI.GLUT
import Control.Concurrent ( MVar, readMVar, swapMVar, newMVar, takeMVar, putMVar, forkIO, forkOS, threadDelay )
import Control.Monad ( unless, forever )

import qualified Data as D
import Util


setup :: D.Options s -> IO ()
setup opts = do
  let cbks = D.callbacks opts
  let wopt = D.windowOptions opts

  (wName, _) <- getArgsAndInitialize
  _ <- makeWindow wName wopt

  state0 <- D.init cbks
  stateMVar <- newMVar state0
  displayReadyMVar <- newMVar False

  let exitOverride a b c d = case (a,b) of
        (Char '\27', Down) -> exitSuccess
        _ -> do
          let kbm = D.keyboardMouse cbks
          stateMVar `withMVar` (\s -> kbm s (a, b, c, d))
          postRedisplay Nothing          

      motionCallback' pos = do
        let mtn = D.motion cbks
        stateMVar `withMVar`  (\s -> mtn s $ D.Motion D.Active pos)
        postRedisplay Nothing

      passiveMotionCallback' pos = do
        let mtn = D.motion cbks
        stateMVar `withMVar` (\s -> mtn s $ D.Motion D.Passive pos)
        postRedisplay Nothing

      displayCallback' = do
        let dsp = D.display cbks
        stateMVar `withMVar` dsp
        displayReadyMVar`swapMVar` True
        
        flush
        swapBuffers
        postRedisplay Nothing
        return ()      

      reshapeCallback' size = do
        let rshp = D.reshape cbks
        stateMVar `withMVar` (\s -> rshp s size)
  
  displayCallback $= displayCallback'
  reshapeCallback $= Just reshapeCallback'
  keyboardMouseCallback $= Just exitOverride
  motionCallback $= Just motionCallback'
  passiveMotionCallback $= Just passiveMotionCallback'
 
  _ <- case D.idle cbks of
         Nothing  -> return ()
         (Just f) -> do
           _ <- forkIO $ idleThread stateMVar displayReadyMVar f
           return ()

  mainLoop
  where
    withMVar :: MVar s -> (s -> IO s) -> IO ()
    withMVar m f = takeMVar m >>= f >>= putMVar m


makeWindow :: String -> D.WindowOptions -> IO (Window)
makeWindow wName wopt = do
  let size = toSize $ D.windowSize wopt
  let pos  = toPosition $ D.windowPos wopt
  let name = case D.windowName wopt of
               (Just n) -> n
               _        -> wName

  initialDisplayMode    $= [ DoubleBuffered, RGBAMode, WithDepthBuffer ]
  initialWindowSize     $= size
  initialWindowPosition $= pos

  wndw <- createWindow name

  clearColor $= Color4 0 0 0 0
  shadeModel $= Smooth
  depthFunc  $= Just Less
  lighting   $= Enabled

  light (Light 0)   $= Enabled
  ambient (Light 0) $= Color4 1 1 1 1

  materialDiffuse Front   $= Color4 0.5 0.5 0.5 1
  materialSpecular Front  $= Color4 1 1 1 1
  materialShininess Front $= 100
  colorMaterial           $= Just (Front, Diffuse)
  return wndw

idleThread :: MVar s -> MVar Bool -> (s -> IO s) -> IO ()
idleThread stateMVar displayReadyMVar idle = do
  let waitUntilDisplayIsReady :: IO ()
      waitUntilDisplayIsReady = do
        ready <- readMVar displayReadyMVar
        unless ready $ threadDelay 10000 >> waitUntilDisplayIsReady

  waitUntilDisplayIsReady

  forever $ do
    nextState <- readMVar stateMVar >>= idle
    _ <- nextState `seq` swapMVar stateMVar nextState

    postRedisplay Nothing
  



