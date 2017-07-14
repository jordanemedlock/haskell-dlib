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
import qualified Data.ByteString.Char8 as BS

import           Foreign.Ptr

import           Vision.DLib.Types.InlineC
import           Vision.DLib.Types.C
import           Vision.DLib.Types.Array2D
import           Vision.DLib.Types.Shape
import           Vision.DLib.Types.RGBPixel
import           Vision.DLib.Types.Vector
import           Vision.DLib.Types.Rectangle
import           Control.Processor

C.context dlibCtx

C.include "<dlib/gui_core.h>"
C.emitVerbatim "#ifndef DLIB_NO_GUI_SUPPORT"
C.include "<dlib/gui_widgets.h>"
C.emitVerbatim "#endif"
C.include "<dlib/image_processing/render_face_detections.h>"
C.include "typedefs.h"
C.include "<string>"

C.emitVerbatim "#ifndef DLIB_NO_GUI_SUPPORT \n#define GUI(X,Y) X \n#else \n#define GUI(X,Y) Y \n#endif"

C.using "namespace dlib"
C.using "namespace std"

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

-- | Display Image in the given ImageWindow
displayImage :: ImageWindow -> IOProcessor Image Image
displayImage win = processor proc allocator run destroy
  where proc img _ = return img
        allocator img = return img
        run img = do
          winSetImage win img
          return img
        destroy _ = return ()


-- | Adds an overlay to the ImageWindow
displayOverlay :: ImageWindow -> IOProcessor Overlay Overlay
displayOverlay win = processor proc allocator run destroy
  where proc overlay _ = return overlay
        allocator overlay = return overlay
        run overlay = winAddOverlay win overlay >> return overlay
        destroy _ = return ()


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


-- | Adds an overlay to the window.  Uses the Overlay type to overload the C++ add_overlay function.
winAddOverlay :: ImageWindow -> Overlay -> IO ()
winAddOverlay (ImageWindow win) (OverlayRect (rect, color, (Just label))) = let bs = BS.pack label in 
  withPtr rect $ \rPtr -> withPtr color $ \cPtr -> 
    [C.block| void { GUI($(image_window * win)->add_overlay(*$(rectangle * rPtr), *$(rgb_pixel * cPtr), string($bs-ptr:bs, $bs-len:bs));,) }|] 
winAddOverlay (ImageWindow win) (OverlayRect (rect, color, Nothing)) = 
  withPtr rect $ \rPtr -> withPtr color $ \cPtr -> 
    [C.block| void { GUI($(image_window * win)->add_overlay(*$(rectangle * rPtr), *$(rgb_pixel * cPtr));,) }|] 
winAddOverlay (ImageWindow win) (OverlayLine (p1, p2, color)) = 
  withPtr p1 $ \p1Ptr -> withPtr p2 $ \p2Ptr -> withPtr color $ \cPtr -> 
    [C.block| void { GUI($(image_window * win)->add_overlay(*$(point * p1Ptr), *$(point * p2Ptr), *$(rgb_pixel * cPtr));,) }|] 
winAddOverlay (ImageWindow win) (OverlayCircle (c, r, color, (Just label))) = let bs = BS.pack label in let cr = fromIntegral r in 
  withPtr c $ \cePtr -> withPtr color $ \cPtr -> 
    [C.block| void { GUI($(image_window * win)->add_overlay(*$(point * cePtr), *$(int cr), *$(rgb_pixel * cPtr), string($bs-ptr:bs, $bs-len:bs));,) }|] 
winAddOverlay (ImageWindow win) (OverlayCircle (c, r, color, Nothing)) = let cr = fromIntegral r in 
  withPtr c $ \cePtr -> withPtr color $ \cPtr -> 
    [C.block| void { GUI($(image_window * win)->add_overlay(*$(point * cePtr), *$(int cr), *$(rgb_pixel * cPtr));,) }|] 
winAddOverlay (ImageWindow win) (OverlayShape shape) = 
  withPtr shape $ \shPtr -> 
    [C.block| void { GUI($(image_window * win)->add_overlay(*$(full_object_detection * shPtr));,) }|] 
winAddOverlay (ImageWindow win) (OverlayFace (shape, Nothing)) = 
  withPtr shape $ \shPtr -> 
    [C.block| void { GUI($(image_window * win)->add_overlay(render_face_detection(*$(full_object_detection * shPtr)));,) }|]
winAddOverlay (ImageWindow win) (OverlayFace (shape, Just color)) = 
  withPtr shape $ \shPtr -> withPtr color $ \cPtr ->
    [C.block| void { GUI($(image_window * win)->add_overlay(render_face_detection(*$(full_object_detection * shPtr), *$(rgb_pixel * cPtr)));,) }|] 
winAddOverlay win (OverlayRects rects)      = mapM_ (winAddOverlay win . OverlayRect)   rects 
winAddOverlay win (OverlayShapes shapes)    = mapM_ (winAddOverlay win . OverlayShape)  shapes 
winAddOverlay win (OverlayFaces shapes)     = mapM_ (winAddOverlay win . OverlayFace)   shapes 
winAddOverlay win (OverlayLines lines)      = mapM_ (winAddOverlay win . OverlayLine)   lines
winAddOverlay win (OverlayCircles circles)  = mapM_ (winAddOverlay win . OverlayCircle) circles


-- | Overlay configuration data type         
data Overlay = OverlayRect    OverlayRect
             | OverlayShape   OverlayShape
             | OverlayFace    OverlayFace
             | OverlayLine    OverlayLine
             | OverlayCircle  OverlayCircle
             
             | OverlayRects   [OverlayRect]
             | OverlayShapes  [OverlayShape]
             | OverlayFaces   [OverlayFace]
             | OverlayLines   [OverlayLine]
             | OverlayCircles [OverlayCircle]
             deriving Show

-- | Rectangle overlay with a color and an optional label
type OverlayRect = (Rectangle, RGBPixel, Maybe String)

-- | Shape overlay with a color and an optional list of labels
type OverlayShape = Shape -- TODO: add labels 

-- | Shape overlay with a color and an optional list of labels
type OverlayFace = (Shape, Maybe RGBPixel) -- TODO: add labels 

-- | Line overlay with a color
type OverlayLine = (Point, Point, RGBPixel)

-- | Circle overlay with a center, radius, color and optional label string
type OverlayCircle = (Point, Int, RGBPixel, Maybe String)
