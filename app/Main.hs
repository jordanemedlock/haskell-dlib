module Main where

import Vision.DLib
import System.Environment

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
  print shapes
  