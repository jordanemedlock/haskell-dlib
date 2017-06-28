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

C.context dlibCtx

C.include "<dlib/image_processing.h>"
C.include "typedefs.h"

C.using "namespace dlib"

-- | Creates a ShapePredictor
mkShapePredictor :: IO ShapePredictor
mkShapePredictor = ShapePredictor <$> [C.exp| void * { new shape_predictor() }|]

-- | Deserializes a ShapePredictor from a file
deserializeShapePredictor :: ShapePredictor -> String -> IO ()
deserializeShapePredictor (ShapePredictor sp) value = do
  let bs = BS.pack value
  [C.block| void { deserialize($bs-ptr:bs) >> *((shape_predictor *)$(void * sp)); } |]

-- | Runs a ShapePredictor on an image within a rectangle
runShapePredictor :: ShapePredictor -> Image -> Rectangle -> IO Shape
runShapePredictor (ShapePredictor sp) (Image img) rect = do
  alloca $ \rectPtr -> do
    poke rectPtr rect
    let voidPtr = castPtr rectPtr
    -- TODO: fix this
    fromPtr =<< [C.block| full_object_detection * {
      full_object_detection * det = new full_object_detection();
      *det = (*(shape_predictor *)$(void * sp))(*$(image * img), *(rectangle *)$(void * voidPtr));
      return det;
    }|]
