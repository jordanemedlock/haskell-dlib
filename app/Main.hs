{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Main where

import Vision.DLib.Monad
import System.Environment
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad
import Control.Processor
import Control.Processor.Utils
import Control.Arrow
import Control.Monad.IO.Class


detect :: FrontalFaceDetector -> ShapePredictor -> Image -> DLib [Shape]
detect det sp img = do
  rects <- runFrontalFaceDetector det img
  mapM (runShapePredictor sp img) rects

main :: IO ()
main = do
  --
  -- print sizeofShape
  -- print alignofShape
  -- print sizeofRect
  -- print alignofRect
  -- print sizeofVector
  -- print alignofVector

  (spFile:images) <- getArgs -- Needs a shape prodictor you can find one at: http://dlib.net/files/shape_predictor_68_face_landmarks.dat.bz2

  runDLib $ do
    det <- mkFrontalFaceDetector
    sp <- mkShapePredictor spFile
    forM_ images $ \str -> do
      img <- loadImage str
      img <- pyramidUp img
      shapes <- detect det sp img
      liftIO $ print shapes

