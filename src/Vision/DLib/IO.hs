{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}


{-|
Module      : Vision.DLib.IO
Description : Input output functions
Copyright   : (c) Jordan Medlock, 2017
Maintainer  : jordanemedlock@gmail.com
Portability : POSIX

load* functions and save* functions.
-}
module Vision.DLib.IO where

import qualified Language.C.Inline as C
import qualified Data.ByteString.Char8 as BS

import           Vision.DLib.Types.Array2D
import           Vision.DLib.Types.InlineC


C.context dlibCtx


C.include "<string>"

C.include "<dlib/image_processing/frontal_face_detector.h>"
C.include "<dlib/image_processing.h>"
C.include "<dlib/image_io.h>"
C.include "<iostream>"
C.include "typedefs.h"

-- | Loads an image without specifying the image format.
loadImage :: Image -> String -> IO ()
loadImage (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    load_image(*$(image * img), $bs-ptr:bs);
  }|]

-- | Load a bitmap image
loadBMP :: Image -> String -> IO ()
loadBMP (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    load_bmp(*$(image * img), $bs-ptr:bs);
  }|]

-- | Load a DNG file
loadDNG :: Image -> String -> IO ()
loadDNG (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    load_dng(*$(image * img), $bs-ptr:bs);
  }|]

-- | Load a JPEG file
loadJPEG :: Image -> String -> IO ()
loadJPEG (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    load_jpeg(*$(image * img), $bs-ptr:bs);
  }|]

-- | Load a PNG file
loadPNG :: Image -> String -> IO ()
loadPNG (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    load_png(*$(image * img), $bs-ptr:bs);
  }|]

-- | Save to a bitmap file
saveBMP :: Image -> String -> IO ()
saveBMP (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    save_bmp(*$(image * img), $bs-ptr:bs); 
  }|]
  
-- | Save to a DNG file
saveDNG :: Image -> String -> IO ()
saveDNG (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    save_dng(*$(image * img), $bs-ptr:bs); 
  }|]
  
-- | Save to a JPEG file
saveJPEG :: Image -> String -> IO ()
saveJPEG (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    save_jpeg(*$(image * img), $bs-ptr:bs); 
  }|]
  
-- | Save to a PNG file
savePNG :: Image -> String -> IO ()
savePNG (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    save_png(*$(image * img), $bs-ptr:bs); 
  }|]
  