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
  $( image_window * winPtr )->set_image( $(image * imgPtr) );
}|]

-- TODO: Continue
-- winAddFaceDetections :: ImageWindow -> [Shape] -> IO ()
-- winAddFaceDetections (ImageWindow winPtr) shapes = do
