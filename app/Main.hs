{-# LANGUAGE OverloadedStrings #-}

module Main where

import Vision.DLib
import System.Environment
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BS

detectFaces :: FrontalFaceDetector -> ShapePredictor -> String -> IO [Shape]
detectFaces detector shapePredictor image = do
  img <- mkImage
  loadImage img image
  
  rects <- runFrontalFaceDetector detector img
  
  mapM (runShapePredictor shapePredictor img) rects
    

main :: IO ()
main = do
  
  (spFile:images) <- getArgs

  detector <- mkFrontalFaceDetector
  shapePredictor <- mkShapePredictor 
  deserializeShapePredictor shapePredictor spFile
  
  shapes <- mapM (detectFaces detector shapePredictor) images
  print $ length shapes
  print $ ((decode "[{\"x\": 10, \"y\": 10}]") :: Maybe [Point])
  print $ ((decode "[{\"left\": 10, \"top\": 10, \"right\": 10, \"bottom\": 10}]") :: Maybe [Rectangle])
  BS.putStrLn $ encode shapes
  print ((decode $ encodePretty shapes) :: Maybe [[Shape]])
  