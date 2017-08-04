{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Main where

import Vision.DLib
import System.Environment
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad
import Control.Processor
import Control.Processor.Utils
import Control.Arrow


detect :: String -> IOProcessor Image [Shape]
detect sp = (arepeat &&& faceDetector)
        >>> azip
        >>> pmap (
          shapePredictor sp
        )


arepeat :: IOProcessor a [a]
arepeat = arr repeat

azip :: (Monad m) => Processor m ([a],[b]) [(a,b)]
azip = arr (uncurry zip)


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

  win <- mkImageWindow

  shapes <- concat <$> run (pmap $ open >>> displayImage win >>> marr pyramidUp >>> detect spFile) images
  print shapes

  let overlays = OverlayShapes shapes

  run (displayOverlay win) overlays

  char <- getChar
  return ()
