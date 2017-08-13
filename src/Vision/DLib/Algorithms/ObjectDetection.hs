{-# LANGUAGE CPP             #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}


{-|
Module      : Vision.DLib.Algorithms.ObjectDetection
Description : Object detection algorithms
Copyright   : (c) Jordan Medlock, 2017
Maintainer  : jordanemedlock@gmail.com
Portability : POSIX

Contains dlib object detection algorithms.
-}
module Vision.DLib.Algorithms.ObjectDetection
( FrontalFaceDetector(..)
, mkFrontalFaceDetector
, runFrontalFaceDetector
, destroyFrontalFaceDetector
, faceDetector
) where

import qualified Language.C.Inline           as C
import qualified Language.C.Inline.Cpp       as C

import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable

import           Control.Processor
import           Data.IORef
import           Vision.DLib.Types.Array2D
import           Vision.DLib.Types.C
import           Vision.DLib.Types.InlineC
import           Vision.DLib.Types.Rectangle

C.context dlibCtx

C.include "<dlib/image_processing/frontal_face_detector.h>"
C.include "typedefs.h"

C.using "namespace dlib"

-- | Represents a pointer to the c++ type @frontal_face_detector@
newtype FrontalFaceDetector = FrontalFaceDetector (Ptr C'FrontalFaceDetector) deriving Show

-- | Creates a FrontalFaceDetector
mkFrontalFaceDetector :: IO FrontalFaceDetector
mkFrontalFaceDetector = FrontalFaceDetector <$> [C.exp| frontal_face_detector * { new frontal_face_detector(get_frontal_face_detector()) }|]


-- | Creates a FrontalFaceDetector
destroyFrontalFaceDetector :: FrontalFaceDetector -> IO ()
destroyFrontalFaceDetector (FrontalFaceDetector det) = [C.block| void { delete $(frontal_face_detector * det); }|]


-- | Runs a FrontalFaceDetector
runFrontalFaceDetector :: FrontalFaceDetector -> Image -> IO [Rectangle]
runFrontalFaceDetector (FrontalFaceDetector det) (Image img) = do

  rects <- newIORef ([] :: [Rectangle])

  let addRect l t r b = do
        let rect = Rectangle l t r b
        modifyIORef rects (rect:)

  [C.block| void {
    frontal_face_detector * det = $(frontal_face_detector * det);
    array2d<rgb_pixel> * img = $(image * img);
    std::vector<rectangle> rects = (* det)(* img);

    for (int i = 0; i < rects.size(); i++) {
      $fun:(void (*addRect)(long,long,long,long))(
        rects[i].left(),
        rects[i].top(),
        rects[i].right(),
        rects[i].bottom()
      );
    }
  }|]

  readIORef rects



-- | Face Detector IOProcessor
faceDetector :: IOProcessor Image [Rectangle]
faceDetector = processor proc alloc run dest
  where proc img (det, _) = return (det, img)
        alloc img = do
          det <- mkFrontalFaceDetector
          return (det, img)
        run (det, img) = runFrontalFaceDetector det img
        dest (det, _) = destroyFrontalFaceDetector det
