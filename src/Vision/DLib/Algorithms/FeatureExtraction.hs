{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Vision.DLib.Algorithms.FeatureExtraction where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Marshal.Array
import           Foreign.Marshal.Alloc
import qualified Data.ByteString.Char8 as BS

import           Vision.DLib.Types.Array2D
import           Vision.DLib.Types.Rectangle
import           Data.Monoid

C.context (C.cppCtx <> C.bsCtx)

C.include "<dlib/image_processing.h>"

C.using "namespace dlib"

newtype ShapePredictor = ShapePredictor (Ptr ())
newtype Shape = Shape (Ptr ()) deriving Show

shGetRect :: Shape -> IO (Ptr Rectangle)
shGetRect (Shape ptr) = castPtr <$> [C.exp| void * { &((full_object_detection *)$(void * ptr))->get_rect() }|]

shNumParts :: Shape -> IO C.CLong
shNumParts (Shape ptr) = [C.exp| long { ((full_object_detection *)$(void * ptr))->num_parts() }|]

shGetPart :: Shape -> C.CLong -> IO (Ptr Point)
shGetPart (Shape ptr) idx = castPtr <$> [C.exp| void * { ((full_object_detection *)$(void * ptr))->part($(long idx)) }|]

mkShapePredictor = ShapePredictor <$> [C.exp| void * { new shape_predictor() }|]

deserializeShapePredictor (ShapePredictor sp) value = do
  let bs = BS.pack value
  [C.block| void { deserialize($bs-ptr:bs) >> *((shape_predictor *)$(void * sp)); } |]
  
runShapePredictor :: ShapePredictor -> Image -> Rectangle -> IO Shape
runShapePredictor (ShapePredictor sp) (Image img) rect = do
  alloca $ \rectPtr -> do
    poke rectPtr rect
    let voidPtr = castPtr rectPtr
    Shape <$> [C.block| void * {
      full_object_detection * det = new full_object_detection();
      *det = (*(shape_predictor *)$(void * sp))(*(array2d<rgb_pixel> *)$(void * img), *(rectangle *)$(void * voidPtr));
      return det;
    }|]
