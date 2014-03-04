{-# LANGUAGE RankNTypes, TypeFamilies, TypeOperators, FlexibleContexts, NoMonomorphismRestriction #-}

module HPController where

import qualified Data.Vector as V
import HPModel
import Chain
import Coord
import Metro
import Moves


import Beads

import Data.Has
import Control.Concurrent ( MVar, newMVar, takeMVar, putMVar, forkIO, forkOS, threadDelay, isEmptyMVar )

import Disp.Util

import Graphics.UI.GLUT

import System.Random.MWC
import Control.Monad.Primitive

data HPState = HPState
type instance TypeOf HPState = ( V.Vector HPResidue
                               , Chain Coord2d
                               , MVar (Chain Coord2d)
                               , [Double])


residueColor :: HPResidue -> Color4 GLfloat
residueColor H = Color4 0.7 0.7 0.7 1
residueColor P = Color4   1   0   0 1

idleHP :: (Has HPState r, Has Beads r) => GenIO -> Int -> r -> IO r
idleHP gen n r = do
  b <- isEmptyMVar result
  if b
    then do
      return r
    else do
      chs <- takeMVar result
      bds <- updateBeads chs
      forkIO $ simThread n gen chs result residues (take n temps)
      return $ Beads ^= bds $ HPState ^= (residues, chs, result, drop n temps) $ r
  where
    beads = Beads ^. r
    (residues, chain, result, temps) = HPState ^. r
    
    updateBeads chs = do
      let crds = [ (x, y, i)  | c@(Coord2d x y) <- toList chs, let (Just i) = chs `cIndex` c ]
      let (x0', y0', _) = crds !! 0
      let bds = [ (Vector3 (x - x0) (y - y0) 0, residueColor residue) | (x', y', i) <- crds
                                                                      , let x  = toGLfloat $ fromIntegral x'
                                                                      , let y  = toGLfloat $ fromIntegral y'
                                                                      , let x0 = toGLfloat $ fromIntegral x0'
                                                                      , let y0 = toGLfloat $ fromIntegral y0'
                                                                      , let residue = residues V.! i]
      return bds

simThread n gen chs result residues temps = do
  let score ch = - (energy residues ch)
  (x, _) <- metropolisHastings score generateCandidate chs gen temps
  _ <- x `seq` putMVar result x
  return ()


-- Get probability for one list element given uniform distribution
getProb :: [a] -> Double
getProb list = fromRational $ 1 / (fromIntegral $ length list)

-- Randomly select one candidate of all given pullMoves
generateCandidate :: (PrimMonad m)    => 
                    Chain Coord2d     -> 
                    Gen (PrimState m) -> 
                    m (Candidate (Chain Coord2d))
generateCandidate ch gen = do 
    let list = pullMoves ch
    chosenMove <-  pick list gen
    let candidate = after chosenMove
    let prob = getProb list
    let probBack = getProb $ pullMoves candidate
    return $ Candidate candidate (prob, probBack)



