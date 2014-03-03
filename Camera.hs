{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts #-}

module Camera where

import Data.Has
import Graphics.UI.GLUT

import qualified Data as D
import Util


data Camera =
  Camera { phi   :: GLdouble
         , theta :: GLdouble
         , rho   :: GLdouble
         , pos   :: (GLdouble, GLdouble, GLdouble)
         , ballX :: GLint
         , ballY :: GLint
         , leftButton :: GLint
         , rightButton :: GLint
         } deriving (Show, Eq)

data CameraState = CameraState;
type instance TypeOf CameraState = Camera
cameraState = CameraState

initCamera :: (GLdouble, GLdouble, GLdouble) -> Camera
initCamera p = Camera 60 20 70 p (-1) (-1) 0 0

focus :: (Has CameraState r) => r -> IO ()
focus r = do
  lookAt (Vertex3 xc yc zc) (Vertex3 x0 y0 z0) (Vector3 0 0 (-1))
  where
    cam          = CameraState ^. r
    (x0, y0, z0) = pos cam
    phi'         = phi cam
    theta'       = theta cam
    rho'         = rho cam

    xc = x0 + rho'*cos(phi'*pi/180)*cos(theta'*pi/180)
    yc = y0 + rho'*sin(phi'*pi/180)*cos(theta'*pi/180)
    zc = z0 - rho'*sin(theta'*pi/180)

cameraMotion :: (Has CameraState r) => r  -> (D.MotionType, (Int, Int)) -> IO r
cameraMotion r (D.Passive, _)     = return r
cameraMotion r (D.Active, (x, y)) = do
  return $ CameraState ^= nxtCam $ r
  where
    cam@(Camera phi0 theta0 rho0 (x0, y0, z0) bx by lb rb) = CameraState ^. r
    nxtCam = Camera nxtPhi nxtTheta rho0 nxtPos nxtBallX nxtBallY lb rb
    deltaX = if bx == -1 then 0 else fromIntegral (toGLint x - bx)
    deltaY = if by == -1 then 0 else fromIntegral (toGLint y - by)
    nxtTheta'
      | deltaY + theta0 > 80    = 80
      | deltaY + theta0 < (-80) = (-80)
      | otherwise               = deltaY + theta0
    nxtX = x0 + 0.003*rho0*( -sin(phi0*pi/180)*deltaX - cos(phi0*pi/180)*deltaY)
    nxtY = y0 + 0.003*rho0*(  cos(phi0*pi/180)*deltaX - sin(phi0*pi/180)*deltaY)
    (nxtPhi, nxtTheta)
      | lb == 1   = (phi0 + deltaX, nxtTheta')
      | otherwise = (phi0, theta0)
    nxtPos
      | rb == 1   = (nxtX, nxtY, z0)
      | otherwise = (x0, y0, z0)
    nxtBallX = toGLint x
    nxtBallY = toGLint y  


--cameraKeyboardMouse :: (Has CameraState r) => D.KeyboardMouseCallback r
cameraKeyboardMouse :: (Has CameraState r) => r -> D.KM -> IO r
cameraKeyboardMouse r (key, keyState, _, _) = do
  return $ CameraState ^= newCam $ r
  where
    cam    = CameraState ^. r
    newCam = cam { rho = rho', leftButton = lb, rightButton = rb, ballX = bx, ballY = by } 
    (lb, reset0) = case (key, keyState) of
                     (MouseButton LeftButton, Down) -> (1, True)
                     (MouseButton LeftButton, Up)   -> (0, False)
                     _                              -> (leftButton cam, False)
    (rb, reset1) = case (key, keyState) of
                     (MouseButton RightButton, Down) -> (1, True)
                     (MouseButton RightButton, Up)   -> (0, False)
                     _                               -> (rightButton cam, False)
    rho' = case (key, keyState) of
             (MouseButton WheelUp, Down)   -> 0.9 * (rho cam)
             (Char 'e', Down)              -> 0.9 * (rho cam)
             (MouseButton WheelDown, Down) -> 1.1 * (rho cam)
             (Char 'q', Down)              -> 1.1 * (rho cam)
             _                             -> rho cam
    (bx,by) | reset0 || reset1 = (-1, -1)
            | otherwise        = (ballX cam, ballY cam)


cameraReshape :: D.ReshapeCallback s
cameraReshape a s@(w, h) = do
  viewport $= (Position 0 0, toSize s)
  matrixMode $= Projection
  loadIdentity
  perspective 40 (fromIntegral w / fromIntegral h) 0.1 1000
  matrixMode $= Modelview 0
  loadIdentity
  postRedisplay Nothing
  return a



