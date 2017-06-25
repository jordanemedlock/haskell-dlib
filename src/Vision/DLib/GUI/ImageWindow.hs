{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Vision.DLib.GUI.ImageWindow where


import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Marshal.Array
import           Foreign.Marshal.Alloc
import qualified Data.ByteString.Char8 as BS

import           Data.Monoid

import           Vision.DLib.Types.InlineC
import           Vision.DLib.Types.C
import           Vision.DLib.Types.Array2D
import           Vision.DLib.Types.Shape

C.context dlibCtx

C.include "<dlib/gui_widgets.h>"
C.include "<dlib/image_processing/render_face_detections.h>"
C.include "typedefs.h"

C.using "namespace dlib"

newtype ImageWindow = ImageWindow (Ptr C'ImageWindow) deriving Show

mkImageWindow :: IO ImageWindow
mkImageWindow = ImageWindow <$> [C.exp| image_window * { new image_window() }|]

winClearOverlay :: ImageWindow -> IO ()
winClearOverlay (ImageWindow ptr) = [C.block| void {
  $( image_window * ptr )->clear_overlay();
}|]

winSetImage :: ImageWindow -> Image -> IO ()
winSetImage (ImageWindow winPtr) (Image imgPtr) = [C.block| void {
  $( image_window * winPtr )->set_image( *$(image * imgPtr) );
}|]


winAddFaceDetection :: ImageWindow -> Shape -> IO ()
winAddFaceDetection (ImageWindow winPtr) shape = do
  withPtr shape $ \shapePtr -> do
    [C.block| void {
      $(image_window * winPtr)->add_overlay(render_face_detections(*$(full_object_detection * shapePtr)));
    }|]
