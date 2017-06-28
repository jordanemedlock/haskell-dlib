{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

{-|
Module      : Vision.DLib.Types.Shape
Description : DLib Shape type
Copyright   : (c) Jordan Medlock, 2017
Maintainer  : jordanemedlock@gmail.com
Portability : POSIX

DLib Shape type.  Equivalent to the C++ type @full_object_detection@
-}
module Vision.DLib.Types.Shape where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import           Foreign.Ptr
import           Foreign.Marshal.Array

import           Vision.DLib.Types.Rectangle
import           Vision.DLib.Types.Vector
import           Vision.DLib.Types.InlineC
import           Vision.DLib.Types.C
import           Data.Monoid
import           Data.Aeson
import           Control.Monad

C.context dlibCtx

C.include "<dlib/image_processing.h>"
C.include "<iostream>"
C.using "namespace dlib"

-- | Represents a pointer to the C++ type @shape_predictor@
newtype ShapePredictor = ShapePredictor (Ptr ())

type instance C Shape = C'Shape

-- | Represents a @full_object_detection@
data Shape = Shape
  { shParts :: [Point]
  , shRect :: Rectangle
  } deriving Show

instance CSizeOf Shape where
  cSizeOf _ = fromIntegral [C.pure| long { sizeof(dlib::full_object_detection) }|]
  cAlignOf _ = fromIntegral [C.pure| long { alignof(dlib::full_object_detection) }|]


-- instance Storable Shape where
--   sizeOf _ = fromIntegral sizeofShape
--   alignment _ = fromIntegral alignofShape
--   peek ptr = fromPtr (castPtr ptr)
--   poke ptr (Shape ps r) = do
--     withArrayLen ps $ \len arr -> do
--       vec <- [C.exp| void * { new std::vector<point>($(point * arr), $(point * arr) + $(long len)) }|]
--

instance WithPtr Shape where
  withPtr (Shape ps r) func = do
    withPtr r $ \rectPtr -> do
      withArrayLen ps $ \len arr -> do
        let arrLen = fromIntegral len
        let arrPtr = castPtr arr
        ptr <- [C.block| full_object_detection * {
          rectangle rect;
          std::vector<point> points($(point * arrPtr), $(point * arrPtr) + $(long arrLen));
          return new full_object_detection(
            *$(rectangle * rectPtr),
            points
          );
        }|]

        ret <- func ptr

        [C.block| void { delete $( full_object_detection * ptr); }|]

        return ret

instance FromPtr Shape where
  fromPtr ptr = do
    rect <- fromPtr =<< [C.exp| rectangle * { &$(full_object_detection * ptr)->get_rect() }|]
    numParts <- [C.exp| long { $(full_object_detection * ptr)->num_parts() }|]

    parts <- withPtr (Point 0 0) $ \elemPtr -> do
      forM [0..(numParts-1)] $ \i -> do
        [C.block| void {
          *$(point * elemPtr) = $(full_object_detection * ptr)->part($(long i));
        }|]
        fromPtr $ castPtr elemPtr
    return $ Shape parts rect

instance ToJSON Shape where
  toJSON (Shape parts rect) = object ["parts" .= parts, "rect" .= rect]
  toEncoding (Shape parts rect) = pairs ("parts" .= parts <> "rect" .= rect)

instance FromJSON Shape where
  parseJSON = withObject "shape" $ \o -> do
    parts <- o .: "parts"
    rect <- o.: "rect"
    return $ Shape parts rect
