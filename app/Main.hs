{-# LANGUAGE OverloadedStrings #-}

module Main where

import Vision.DLib
import System.Environment
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad



detectFaces :: FrontalFaceDetector -> ShapePredictor -> String -> IO [Shape]
detectFaces detector shapePredictor image = do
  img <- mkImage
  loadImage img image

  rects <- runFrontalFaceDetector detector img

  shapes <- mapM (runShapePredictor shapePredictor img) rects

  win <- mkImageWindow
  winClearOverlay win
  winSetImage win img

  forM_ shapes $ \shape -> do
    winAddFaceDetection win shape

  return shapes


main :: IO ()
main = do
  --
  -- print sizeofShape
  -- print alignofShape
  -- print sizeofRect
  -- print alignofRect
  -- print sizeofVector
  -- print alignofVector

  (spFile:images) <- getArgs

  detector <- mkFrontalFaceDetector
  shapePredictor <- mkShapePredictor
  deserializeShapePredictor shapePredictor spFile

  shapes <- mapM (detectFaces detector shapePredictor) images
  char <- getChar
  return ()
