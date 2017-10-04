{-# LANGUAGE CPP             #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}


{-|
Module      : Vision.DLib.Types.Array2D
Description : Array2D type
Copyright   : (c) Jordan Medlock, 2017
Maintainer  : jordanemedlock@gmail.com
Portability : POSIX

The Array2D type represents a dlib image.
-}
module Vision.DLib.Types.Array2D where


import           Foreign.Ptr
import qualified Language.C.Inline         as C
import qualified Language.C.Inline.Cpp     as C

import           Vision.DLib.Types.C
import           Vision.DLib.Types.Rectangle
import           Vision.DLib.Types.InlineC

C.context dlibCtx

C.include "<string>"

C.include "<dlib/image_processing/frontal_face_detector.h>"
C.include "<dlib/image_processing.h>"
C.include "<dlib/image_io.h>"
C.include "<iostream>"
C.include "typedefs.h"


C.using "namespace dlib"
C.using "namespace std"

-- | Represents a pointer to the C++ array_2d<rgb_pixel> type.
newtype Image = Image (Ptr C'Image) deriving Show

-- | Creates an empty image pointer
mkImage :: IO Image
mkImage = Image <$> [C.exp| image * { new array2d<rgb_pixel>() }|]

destroyImage :: Image -> IO ()
destroyImage (Image img) = [C.block| void { delete $( image * img ); }|]

-- | Upscales an image.  Goto <http://dlib.net/imaging.html#pyramid_up> for documentation.
-- | Approximately doubles the image size
pyramidUp :: Image -> IO Image
pyramidUp (Image img) = Image <$> [C.block| image * {
    array2d<rgb_pixel> * img = $(image * img);
    pyramid_up(*img);
    return img;
}|]


imgWidth :: Image -> C.CULong
imgWidth (Image img) = [C.pure| unsigned long { $(image * img)->nc() }|]

imgHeight :: Image -> C.CULong
imgHeight (Image img) = [C.pure| unsigned long { $(image * img)->nr() }|]

-- subImage :: Rectangle -> Image -> IO Image
-- subImage rect (Image img) = Image <$> withPtr rect $ \rPtr -> [C.exp| image * { sub_image($(image * img), $(rectangle * rPtr)) } |]


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
