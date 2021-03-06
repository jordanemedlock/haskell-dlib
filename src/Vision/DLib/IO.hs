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
import qualified Language.C.Inline.Cpp as C
import qualified Data.ByteString.Char8 as BS

import           Vision.DLib.Types.Array2D
import           Vision.DLib.Types.InlineC
import           Control.Processor
import           System.FilePath.Posix ( takeExtension )
import           Data.Char

C.context dlibCtx


C.include "<string>"

C.include "<dlib/image_processing/frontal_face_detector.h>"
C.include "<dlib/image_processing.h>"
C.include "<dlib/image_io.h>"
C.include "<iostream>"
C.include "typedefs.h"

C.using "namespace std"
C.using "namespace dlib"

data FileType = BMP | DNG | JPEG | PNG deriving Show


-- | Saves an image using its file extension
saveImage :: Image -> FilePath -> IO ()
saveImage img filename = case (toLower <$> takeExtension filename) of
  ".bmp" -> savePNG img filename
  ".dng" -> saveDNG img filename
  ".jpg" -> saveJPEG img filename
  ".jpeg" -> saveJPEG img filename
  ".png" -> savePNG img filename
  _ -> error "unknown filetype"

-- | IOProcessor that loads an image from file
open :: IOProcessor String Image
open = processor iter alloc run dest
  where iter fName (img, _) = return (img, fName)
        alloc fName = do
          img <- mkImage
          return (img, fName)
        run (img, fName) = do
          loadImage img fName
          return img
        dest (img, _) = destroyImage img

-- | IOProcessor which saves an image to a filename
save :: IOProcessor (String, Image) ()
save = processor iter alloc run dest
  where iter (fname, img) _ = return (fname, img)
        alloc (fname, img) = return (fname, img)
        run (fname, img) = saveImage img fname
        dest _ = return ()

-- | Loads an image without specifying the image format.
loadImage :: Image -> String -> IO ()
loadImage (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    load_image(*$(image * img), string($bs-ptr:bs, $bs-len:bs));
  }|]

-- | Load a bitmap image
loadBMP :: Image -> String -> IO ()
loadBMP (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    load_bmp(*$(image * img), string($bs-ptr:bs, $bs-len:bs));
  }|]

-- | Load a DNG file
loadDNG :: Image -> String -> IO ()
loadDNG (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    load_dng(*$(image * img), string($bs-ptr:bs, $bs-len:bs));
  }|]

-- | Load a JPEG file
loadJPEG :: Image -> String -> IO ()
loadJPEG (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    load_jpeg(*$(image * img), string($bs-ptr:bs, $bs-len:bs));
  }|]

-- | Load a PNG file
loadPNG :: Image -> String -> IO ()
loadPNG (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    load_png(*$(image * img), string($bs-ptr:bs, $bs-len:bs));
  }|]

-- | Save to a bitmap file
saveBMP :: Image -> String -> IO ()
saveBMP (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    save_bmp(*$(image * img), string($bs-ptr:bs, $bs-len:bs));
  }|]

-- | Save to a DNG file
saveDNG :: Image -> String -> IO ()
saveDNG (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    save_dng(*$(image * img), string($bs-ptr:bs, $bs-len:bs));
  }|]

-- | Save to a JPEG file
saveJPEG :: Image -> String -> IO ()
saveJPEG (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    save_jpeg(*$(image * img), string($bs-ptr:bs, $bs-len:bs));
  }|]

-- | Save to a PNG file
savePNG :: Image -> String -> IO ()
savePNG (Image img) fname = do
  let bs = BS.pack fname
  [C.block| void {
    save_png(*$(image * img), string($bs-ptr:bs, $bs-len:bs));
  }|]
