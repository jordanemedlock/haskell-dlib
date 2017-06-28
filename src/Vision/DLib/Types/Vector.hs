{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

{-|
Module      : Vision.DLib.Types.Vector
Description : DLib Point type
Copyright   : (c) Jordan Medlock, 2017
Maintainer  : jordanemedlock@gmail.com
Portability : POSIX

Represents an x,y coordinate.
-}
module Vision.DLib.Types.Vector 
( Point(..)
) where


import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Vision.DLib.Types.C
import Vision.DLib.Types.InlineC
import Vision.DLib.Types.Constants
import Data.Aeson
import Data.Monoid

C.context dlibCtx

C.include "<dlib/geometry.h>"

C.using "namespace dlib"

-- | Represents a coordinate on the xy-plane
data Point = Point 
  { ptX :: CLong
  , ptY :: CLong
  } deriving Show

type instance C Point = C'Point

alloca_point :: (Ptr C'Point -> IO a) -> IO a
alloca_point f = do
  ptr <- [C.exp| point * { new point() }|]
  
  ret <- f ptr
  
  [C.block| void { delete $(point * ptr); }|]
  
  return ret

instance Storable Point where
  sizeOf _ = fromIntegral sizeofPoint
  alignment _ = fromIntegral alignofPoint
  peek ptr = do
    let longPtr = castPtr ptr :: Ptr CLong
    x <- peekElemOff longPtr 0
    y <- peekElemOff longPtr 1
    return $ Point x y
  poke ptr (Point x y) = do
    let longPtr = castPtr ptr :: Ptr CLong
    pokeElemOff longPtr 0 x
    pokeElemOff longPtr 1 y

instance WithPtr Point where
  withPtr (Point x y) func = do
    alloca_point $ \ptr -> do
      [C.block| void {
        $(point * ptr)->x() = $(long x);         
        $(point * ptr)->y() = $(long y);
      }|]
      func ptr

instance FromPtr Point where 
  fromPtr ptr = do
    x <- [C.exp| long { $(point * ptr)->x() }|]
    y <- [C.exp| long { $(point * ptr)->y() }|]
    return $ Point x y

instance ToJSON Point where
  toJSON (Point x y) = object ["x" .= (fromIntegral x :: Int), "y" .= (fromIntegral y :: Int)]
  toEncoding (Point x y) = pairs ("x" .= (fromIntegral x :: Int) <> "y" .= (fromIntegral y :: Int))

toCLong :: Int -> CLong
toCLong = fromIntegral

instance FromJSON Point where
  parseJSON = withObject "Point" $ \o -> do
    x <- toCLong <$> o .: "x"
    y <- toCLong <$> o .: "y"
    return $ Point x y
