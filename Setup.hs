
module Setup ( setup ) where


import Prelude hiding ( init )
import System.Exit ( exitSuccess )
import Data.IORef ( newIORef )
import Graphics.UI.GLUT
import Data.Time.Clock ( getCurrentTime, diffUTCTime, addUTCTime )
import Control.Concurrent ( MVar, readMVar, swapMVar, newMVar, takeMVar, putMVar, forkIO, forkOS, threadDelay )
import Control.Monad ( unless, forever, foldM )

import qualified Data as D
import Util


setup :: [D.Callback s] -> s -> D.WindowOptions -> IO ()
setup cbks state0 wopt = do
  (wName, _) <- getArgsAndInitialize
  _ <- makeWindow wName wopt
  stateMVar <- newMVar state0
  displayReadyMVar <- newMVar False
  initialise cbks stateMVar 
  setupCallbacks cbks stateMVar displayReadyMVar
  setupIdles cbks stateMVar displayReadyMVar
  mainLoop


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


initialise :: [D.Callback s] -> MVar s -> IO ()
initialise cbks stateMVar = do
  let inits = [f | D.Init f <- cbks]
  unless (null inits) $ do
    state <- takeMVar stateMVar
    state' <- foldM (flip ($)) state inits
    stateMVar `putMVar` state'


setupCallbacks :: [D.Callback s] -> MVar s -> MVar Bool -> IO ()
setupCallbacks cbks stateMVar displayReadyMVar= do
  let exitOverride a b c d = case (a,b) of
        (Char '\27', Down) -> exitSuccess
        _ -> do
          let kbms = [f | D.KeyboardMouse f <- cbks]
          unless (null kbms) $ do
            state  <- takeMVar stateMVar
            state' <- foldM (\s f -> f s (a,b,c,d)) state kbms
            stateMVar `putMVar` state'
  
      motionCallback' pos = do
        let mtns = [f | D.Motion f <- cbks]
        unless (null mtns) $ do
          let pos' = fromPosition pos
          state <- takeMVar stateMVar
          state' <- foldM (\s f -> f s (D.Active, pos')) state mtns
          stateMVar `putMVar` state'

      passiveMotionCallback' pos = do
        let mtns = [f | D.Motion f <- cbks]
        unless (null mtns) $ do
          let pos' = fromPosition pos
          state <- takeMVar stateMVar
          state' <- foldM (\s f -> f s (D.Passive, pos')) state mtns
          stateMVar `putMVar` state'

      displayCallback' = do
        let dsps = [f | D.Display f <- cbks]
        unless (null dsps) $ preservingMatrix $ do 
          state <- readMVar stateMVar
          mapM_ ($ state) dsps
        displayReadyMVar `swapMVar`True
        flush
        swapBuffers
        postRedisplay Nothing
        return ()
      
      reshapeCallback' size = do
        let rsps = [f | D.Reshape f <- cbks]
        unless (null rsps) $ do
          let size' = fromSize size
          state <- takeMVar stateMVar
          state' <- foldM (\s f -> f s size') state rsps
          stateMVar `putMVar` state'

  displayCallback       $= displayCallback'
  reshapeCallback       $= Just reshapeCallback'
  keyboardMouseCallback $= Just exitOverride
  motionCallback        $= Just motionCallback'
  passiveMotionCallback $= Just passiveMotionCallback'

  
setupIdles :: [D.Callback s] -> MVar s -> MVar Bool -> IO ()
setupIdles cbks stateMVar displayReadyMVar = do
  let idls  = [(f, dt) | D.Idle f dt <- cbks]
  mapM_ (\(f, dt) -> forkIO $ idleThread stateMVar displayReadyMVar f dt) idls


idleThread :: MVar s -> MVar Bool -> (s -> IO s) -> Double -> IO ()
idleThread stateMVar displayReadyMVar idle dTime = do
  let waitUntilDisplayIsReady :: IO ()
      waitUntilDisplayIsReady = do
        ready <- readMVar displayReadyMVar
        unless ready $ threadDelay 10000 >> waitUntilDisplayIsReady
  
  waitUntilDisplayIsReady
  
  t0 <- getCurrentTime
  lastTimeRef <- newIORef t0

  forever $ do
    currentTime <- getCurrentTime
    lastTime <- get lastTimeRef

    let usRemaining :: Int
        usRemaining = round $ 1e6 * (dTime - realToFrac (diffUTCTime currentTime lastTime))


    if usRemaining <= 0
      then do
        lastTimeRef $= addUTCTime (realToFrac dTime) lastTime
        nextState <- readMVar stateMVar >>= idle
        _ <- nextState `seq` swapMVar stateMVar nextState
               
        postRedisplay Nothing
      else threadDelay usRemaining







