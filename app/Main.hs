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
detect sp = (arepeat &&& faceDetector) >>> azip >>> pmap (shapePredictor sp)


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

  (spFile:images) <- getArgs

  shapes <- concat <$> run (pmap $ open >>> marr pyramidUp >>> detect spFile) images
  print shapes

  char <- getChar
  return ()
