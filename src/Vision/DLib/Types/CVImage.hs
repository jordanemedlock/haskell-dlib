{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}


{-|
Module      : Vision.DLib.Algorithms.FeatureExtraction
Description : Feature extraction algorithms
Copyright   : (c) Jordan Medlock, 2017
Maintainer  : jordanemedlock@gmail.com
Portability : POSIX

Contains dlib feature extraction algorithms.
-}
module Vision.DLib.Algorithms.FeatureExtraction
( mkShapePredictor
, deserializeShapePredictor
, runShapePredictor
) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Marshal.Alloc
import qualified Data.ByteString.Char8 as BS

import           Vision.DLib.Types.Array2D
import           Vision.DLib.Types.Rectangle
import           Vision.DLib.Types.InlineC
import           Vision.DLib.Types.C
import           Vision.DLib.Types.Shape
