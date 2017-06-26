{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Vision.DLib.IO where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Internal as C
import qualified Language.C.Inline.Cpp as C
import qualified Data.ByteString.Char8 as BS
import           Foreign.Ptr
import           Foreign.Marshal.Array
import           Data.Monoid

import           Vision.DLib.Types.RGBPixel
import           Vision.DLib.Types.Array2D
import           Vision.DLib.Types.C
import           Vision.DLib.Types.InlineC


C.context dlibCtx


C.include "<string>"

C.include "<dlib/image_processing/frontal_face_detector.h>"
C.include "<dlib/image_processing.h>"
C.include "<dlib/image_io.h>"
C.include "<iostream>"
C.include "typedefs.h"


loadImage :: Image -> String -> IO ()
loadImage (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    load_image(*$(image * img), $bs-ptr:bs);
  }|]

loadBMP :: Image -> String -> IO ()
loadBMP (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    load_bmp(*$(image * img), $bs-ptr:bs);
  }|]

loadDNG :: Image -> String -> IO ()
loadDNG (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    load_dng(*$(image * img), $bs-ptr:bs);
  }|]

loadJPEG :: Image -> String -> IO ()
loadJPEG (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    load_jpeg(*$(image * img), $bs-ptr:bs);
  }|]

loadPNG :: Image -> String -> IO ()
loadPNG (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    load_png(*$(image * img), $bs-ptr:bs);
  }|]

saveBMP :: Image -> String -> IO ()
saveBMP (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    save_bmp(*$(image * img), $bs-ptr:bs); 
  }|]
  
saveDNG :: Image -> String -> IO ()
saveDNG (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    save_dng(*$(image * img), $bs-ptr:bs); 
  }|]
  
saveJPEG :: Image -> String -> IO ()
saveJPEG (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    save_jpeg(*$(image * img), $bs-ptr:bs); 
  }|]
  
savePNG :: Image -> String -> IO ()
savePNG (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    save_png(*$(image * img), $bs-ptr:bs); 
  }|]
  