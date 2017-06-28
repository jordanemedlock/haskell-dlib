{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}


{-|
Module      : Vision.DLib.GUI.ImageWindow
Description : Image Window type
Copyright   : (c) Jordan Medlock, 2017
Maintainer  : jordanemedlock@gmail.com
Portability : POSIX

Displays an image in its own window.  Must have gui support and not have the 
DLIB_NO_GUI_SUPPORT flag declared.
-}
module Vision.DLib.GUI.ImageWindow where


import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Internal as C

import           Foreign.Ptr

import           Vision.DLib.Types.InlineC
import           Vision.DLib.Types.C
import           Vision.DLib.Types.Array2D
import           Vision.DLib.Types.Shape

C.context dlibCtx

C.include "<dlib/gui_core.h>"
C.emitVerbatim "#ifndef DLIB_NO_GUI_SUPPORT"
C.include "<dlib/gui_widgets.h>"
C.emitVerbatim "#endif"
C.include "<dlib/image_processing/render_face_detections.h>"
C.include "typedefs.h"

C.using "namespace dlib"

-- | Represents a pointer to the C++ type @image_window@
newtype ImageWindow = ImageWindow (Ptr C'ImageWindow) deriving Show


-- | Creates an @ImageWindow@
mkImageWindow :: IO ImageWindow
mkImageWindow = ImageWindow <$> [C.block| image_window * {
#ifndef DLIB_NO_GUI_SUPPORT
  return new image_window();
#endif
  return 0;
}|]

-- | Clears the @ImageWindow@
winClearOverlay :: ImageWindow -> IO ()
winClearOverlay (ImageWindow ptr) = [C.block| void {
#ifndef DLIB_NO_GUI_SUPPORT
  $( image_window * ptr )->clear_overlay();
#endif
}|]

-- | Sets the image to display in the @ImageWindow@
winSetImage :: ImageWindow -> Image -> IO ()
winSetImage (ImageWindow winPtr) (Image imgPtr) = [C.block| void {
#ifndef DLIB_NO_GUI_SUPPORT
  $( image_window * winPtr )->set_image( *$(image * imgPtr) );
#endif
}|]

-- | Displays a face detection @Shape@ 
winAddFaceDetection :: ImageWindow -> Shape -> IO ()
winAddFaceDetection (ImageWindow winPtr) shape = do
  withPtr shape $ \shapePtr -> do
    [C.block| void {
    #ifndef DLIB_NO_GUI_SUPPORT
      $(image_window * winPtr)->add_overlay(render_face_detections(*$(full_object_detection * shapePtr)));
    #endif
    }|]
