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
( ShapePredictor(..)
, mkShapePredictor
, destroyShapePredictor
, deserializeShapePredictor
, runShapePredictor
, shapePredictor
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
import           Control.Processor

C.context dlibCtx

C.include "<dlib/image_processing.h>"
C.include "typedefs.h"

C.using "namespace dlib"

-- | Represents a pointer to the C++ type @shape_predictor@
newtype ShapePredictor = ShapePredictor (Ptr C'ShapePredictor)


-- | Creates a ShapePredictor
mkShapePredictor :: IO ShapePredictor
mkShapePredictor = ShapePredictor <$> [C.exp| shape_predictor * { new shape_predictor() }|]

-- | Destroys a ShapePredictor
destroyShapePredictor :: ShapePredictor -> IO ()
destroyShapePredictor (ShapePredictor sp) = [C.block| void { delete $(shape_predictor * sp); } |]

-- | Deserializes a ShapePredictor from a file
deserializeShapePredictor :: ShapePredictor -> String -> IO ()
deserializeShapePredictor (ShapePredictor sp) filename = do
  let bs = BS.pack filename
  [C.block| void { deserialize($bs-ptr:bs) >> *$(shape_predictor * sp); } |]

-- | Runs a ShapePredictor on an image within a rectangle
runShapePredictor :: ShapePredictor -> Image -> Rectangle -> IO Shape
runShapePredictor (ShapePredictor sp) (Image img) rect = do
  alloca $ \rectPtr -> do
    poke rectPtr rect
    let voidPtr = castPtr rectPtr
    -- TODO: fix this
    fromPtr =<< [C.block| full_object_detection * {
      full_object_detection * det = new full_object_detection();
      *det = (*$(shape_predictor * sp))(*$(image * img), *$(rectangle * voidPtr));
      return det;
    }|]


-- | Shape Predictor IOProcessor
shapePredictor :: String -> IOProcessor (Image, Rectangle) Shape
shapePredictor filename = processor proc allocator run destroy
  where proc (img, rect) (sp, _, _) = return (sp, img, rect) -- <$> runShapePredictor sp img rect
        allocator (img, rect) = do
          sp <- mkShapePredictor
          deserializeShapePredictor sp filename
          return (sp, img, rect)
        run (sp, img, rect) = runShapePredictor sp img rect
        destroy (sp, _, _) = destroyShapePredictor sp
