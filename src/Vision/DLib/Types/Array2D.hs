{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Vision.DLib.Types.Array2D where


import qualified Language.C.Inline as C
import qualified Language.C.Inline.Internal as C
import qualified Language.C.Inline.Cpp as C
import qualified Data.ByteString.Char8 as BS
import           Foreign.Ptr
import           Foreign.Marshal.Array
import           Data.Monoid

import           Vision.DLib.Types.RGBPixel
import           Vision.DLib.Types.C
import           Vision.DLib.Types.InlineC


C.context dlibCtx


C.include "<string>"

C.include "<dlib/image_processing/frontal_face_detector.h>"
C.include "<dlib/image_processing.h>"
C.include "<dlib/image_io.h>"
C.include "typedefs.h"


C.using "namespace dlib"
C.using "namespace std"

newtype Image = Image (Ptr C'Image)

mkImage = Image <$> [C.exp| image * { new array2d<rgb_pixel>() }|]

pyramidUp :: Image -> IO Image
pyramidUp (Image img) = Image <$> [C.block| image * {
    array2d<rgb_pixel> * img = $(image * img);
    pyramid_up(*img);
    return img;
}|]


{-
data Array2D a = Array2D
  { arrData :: Ptr a
  , arrNCol :: CLong
  , arrNRow :: CLong
  , arrCur :: Ptr a
  , arrLast :: Ptr a
  , arrAtStart :: Ptr a
  }

instance Storable (Array2D a) where
  sizeOf _ = 64
  alignment _ = 8
  peek ptr = do
    d <- peekByteOff ptr 0
    c <- peekByteOff ptr 8
    r <- peekByteOff ptr 16
    cur <- peekByteOff ptr 24
    l <- peekByteOff ptr 32
    a <- peekByteOff ptr 40
    return $ Array2D d c r cur l a
  poke ptr (Array2D d c r cur l a) = do
    pokeByteOff ptr 0 d
    pokeByteOff ptr 8 c
    pokeByteOff ptr 16 r
    pokeByteOff ptr 24 cur
    pokeByteOff ptr 32 l
    pokeByteOff ptr 40 a
-}
