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
import           Control.Processor

C.context dlibCtx

C.include "<dlib/gui_core.h>"
C.emitVerbatim "#ifndef DLIB_NO_GUI_SUPPORT"
C.include "<dlib/gui_widgets.h>"
C.emitVerbatim "#endif"
C.include "<dlib/image_processing/render_face_detections.h>"
C.include "typedefs.h"

C.emitVerbatim "#ifndef DLIB_NO_GUI_SUPPORT \n#define GUI(X,Y) X \n#else \n#define GUI(X,Y) Y \n#endif"

C.using "namespace dlib"

-- | Checks if DLIB_NO_FUI_SUPPORT is defined
hasGUISupport :: Bool
hasGUISupport = (==1) [C.pure| int { GUI(1,0) }|]


-- | Represents a pointer to the C++ type @image_window@
newtype ImageWindow = ImageWindow (Ptr C'ImageWindow) deriving Show


-- | Creates an @ImageWindow@
mkImageWindow :: IO ImageWindow
mkImageWindow = ImageWindow <$> [C.block| image_window * {
  return GUI(new image_window(), 0);
}|]


-- | Destroys an @ImageWindow@
destroyImageWindow :: ImageWindow -> IO ()
destroyImageWindow (ImageWindow win) = [C.block| void {
  GUI(delete $(image_window * win),);
}|]

-- | Clears the @ImageWindow@
winClearOverlay :: ImageWindow -> IO ()
winClearOverlay (ImageWindow ptr) = [C.block| void {
  GUI($( image_window * ptr )->clear_overlay(),);
}|]

-- | Sets the image to display in the @ImageWindow@
winSetImage :: ImageWindow -> Image -> IO ()
winSetImage (ImageWindow winPtr) (Image imgPtr) = [C.block| void {
  GUI($( image_window * winPtr )->set_image( *$(image * imgPtr) ),);
}|]

-- | Displays a face detection @Shape@
winAddFaceDetection :: ImageWindow -> Shape -> IO ()
winAddFaceDetection (ImageWindow winPtr) shape = do
  withPtr shape $ \shapePtr -> do
    [C.block| void {
      GUI($(image_window * winPtr)->add_overlay(render_face_detections(*$(full_object_detection * shapePtr))),);
    }|]

-- | IOSink which displays an image
imageWindow :: IOSink Image
imageWindow = processor iter alloc run dest
  where iter img (win, _) = return (win, img)
        alloc img = do
          win <- mkImageWindow
          return (win, img)
        run (win, img) = do
          winClearOverlay win
          winSetImage win img
        dest (win, _) = destroyImageWindow win

{-
winAddOverlay :: ImageWindow -> Overlay -> IO ()
winAddOverlay (ImageWindow win) (OverlayRect (rect, color, (Just label))) = 
  [C.block| ] 


-- | Overlay configuration data type         
data Overlay = OverlayRect    OverlayRect
             | OverlayShape   OverlayShape
             | OverlayLine    OverlayLine
             | OverlayCircle  OverlayCircle
             
             | OverlayRects   [OverlayRect]
             | OverlayShapes  [OverlayShape]
             | OverlayLines   [OverlayLine]
             | OverlayCircles [OverlayCircle]
             deriving Show

-- | Rectangle overlay with a color and an optional label
type OverlayRect = (Rectangle, Color, Maybe String)

-- | Shape overlay with a color and an optional list of labels
type OverlayObject = (Shape, Color, Maybe [String])

-- | Line overlay with a color
type OverlayLine = (Point, Point, Color)

-- | Circle overlay with a center, radius, color and optional label string
type OverlayCircle = (Point, Int, Color, Maybe String)

-}