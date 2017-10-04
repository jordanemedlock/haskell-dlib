{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}


{-|
Module      : Vision.DLib.OpenCV
Description : OpenCV Iterfacing functions
Copyright   : (c) Jordan Medlock, 2017
Maintainer  : jordanemedlock@gmail.com
Portability : POSIX

-}
module Vision.DLib.OpenCV where


import qualified Language.C.Inline as C

import           Vision.DLib.Types.Array2D
import           Vision.DLib.Types.InlineC
import           Foreign.Ptr

C.context dlibCtx


C.include "<string>"

C.include "<dlib/image_processing.h>"
C.include "<dlib/opencv.h>"
C.include "<iostream>"
C.include "typedefs.h"

fromIplImage :: Ptr () -> IO Image
fromIplImage cvImage = Image <$> [C.block| image * {
  image * img = new dlib::array2d<dlib::rgb_pixel>();
  dlib::cv_image<dlib::bgr_pixel> cvImage((IplImage *)$(void * cvImage));
  dlib::assign_image(*img, cvImage);
  return img;
}|]

