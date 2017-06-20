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
import           Vision.DLib.Types.Vector
import           Vision.DLib.Types.InlineC
import           Data.Monoid

C.context dlibCtx

C.include "<dlib/image_processing.h>"

C.using "namespace dlib"

newtype ShapePredictor = ShapePredictor (Ptr ())

type instance C Shape = C'Shape

data Shape = Shape 
  { shParts :: [Point]
  , shRect :: Rectangle
  } deriving Show

instance WithPtr Shape where
  withPtr (Shape ps r) func = do
    let vec = _ ps
    withPtr r $ \rectPtr -> do
      ptr <- [C.block| full_object_detection * {
        full_object_detecion * det = new full_object_detection(
          $(rectangle * rectPtr), $(vector * vec)
        );
        return det;
      }|]
      
      ret <- func ptr
      
      [C.block| void { delete $( full_object_detection * ptr);}]
      
      return ret
    
    

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
