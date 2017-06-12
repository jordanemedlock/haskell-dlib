{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Vision.DLib.Algorithms.ObjectDetection where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import           Foreign.Ptr
import           Foreign.Marshal.Array

import           Vision.DLib.Types.Array2D

C.context C.cppCtx

C.include "<dlib/image_processing/frontal_face_detector.h>"

C.using "namespace dlib"

newtype FrontalFaceDetector = FrontalFaceDetector (Ptr ())

mkFrontalFaceDetector = FrontalFaceDetector <$> [C.exp| void * { new frontal_face_detector(get_frontal_face_detector()) }|]

runFaceDetector (FrontalFaceDetector det) (Image img) = do
  (n, voidPtr) <- C.withPtrs_ $ \(intPtr, dblPtr) -> [C.block| void {
    frontal_face_detector * det = (frontal_face_detector *)$(void * det);
    array2d<rgb_pixel> * img = (array2d<rgb_pixel> *)$(void * img);
    std::vector<rectangle> rects = (* det)(* img);
    (*$(int * intPtr)) = rects.size();
    (*$(void ** dblPtr)) = &rects[0];
  }|]
  peekArray (fromIntegral n) (castPtr voidPtr)
  