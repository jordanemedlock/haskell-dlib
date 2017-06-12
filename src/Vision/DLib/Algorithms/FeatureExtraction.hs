{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Vision.DLib.Algorithms.FeatureExtraction where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import           Foreign.Ptr
import           Foreign.Marshal.Array

import           Vision.DLib.Types.Array2D

C.context C.cppCtx

C.include "<dlib/image_processing.h>"

C.using "namespace dlib"

newtype ShapePredictor = ShapePredictor (Ptr ())

mkShapePredictor = ShapePredictor <$> [C.exp| void * { new shape_predictor() }|]

deserializeShapePredictor str (ShapePredictor sp) = do
  let bs = BS.pack value
  [C.block| void {
    deserialize($bs-ptr:bs) >> $((shape_predictor)$(void * sp));    
  }]
  return sp
 
loadShapePredictor str = mkShapePredictor >>= deserializeShapePredictor str
