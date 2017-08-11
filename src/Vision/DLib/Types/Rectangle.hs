{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

{-|
Module      : Vision.DLib.Types.Rectangle
Description : DLib Rectangle type
Copyright   : (c) Jordan Medlock, 2017
Maintainer  : jordanemedlock@gmail.com
Portability : POSIX

DLib Rectangle type
-}
module Vision.DLib.Types.Rectangle
( Rectangle(..), rectWidth, rectHeight
, getBoundingRect
, cPrintRect
) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Vision.DLib.Types.C
import Vision.DLib.Types.Vector
import Vision.DLib.Types.InlineC
import Data.Aeson
import Data.Monoid

C.context dlibCtx
C.include "<dlib/geometry.h>"
C.include "<iostream>"

C.using "namespace dlib"

-- | Rectangle data type
data Rectangle = Rectangle
  { rectLeft :: CLong
  , rectTop :: CLong
  , rectRight :: CLong
  , rectBottom :: CLong
  } deriving (Eq)


rectWidth rect = abs $ rectLeft rect - rectRight rect
rectHeight rect = abs $ rectBottom rect - rectTop rect

getBoundingRect :: [Point] -> Rectangle
getBoundingRect ps = Rectangle { rectLeft = minimum $ ptX <$> ps
                               , rectTop = minimum $ ptY <$> ps
                               , rectRight = maximum $ ptX <$> ps
                               , rectBottom = maximum $ ptY <$> ps
                               }


instance Show Rectangle where
  show (Rectangle l t r b) = "[("++(show l)++","++(show t)++") ("++(show r)++","++(show b)++")]"

cPrintRect rPtr = [C.block| void {
  std::cout << $(rectangle * rPtr)->left() << " "
            << $(rectangle * rPtr)->top() << " "
            << $(rectangle * rPtr)->right() << " "
            << $(rectangle * rPtr)->bottom() << "\n";
}|]

type instance C Rectangle = C'Rectangle

instance CSizeOf Rectangle where
  cSizeOf _ = fromIntegral [C.pure| long { sizeof(rectangle) }|]
  cAlignOf _ = fromIntegral [C.pure| long { alignof(rectangle) }|]

instance Storable Rectangle where
  sizeOf = cSizeOf
  alignment = cSizeOf
  peek ptr = do
    let longPtr = castPtr ptr :: Ptr CLong
    l <- peekElemOff longPtr 0
    t <- peekElemOff longPtr 1
    r <- peekElemOff longPtr 2
    b <- peekElemOff longPtr 3
    return $ Rectangle l t r b

  poke ptr (Rectangle l t r b) = do
    let longPtr = castPtr ptr :: Ptr CLong
    pokeElemOff longPtr 0 l
    pokeElemOff longPtr 1 t
    pokeElemOff longPtr 2 r
    pokeElemOff longPtr 3 b

instance WithPtr Rectangle where
  withPtr rect func = do
    alloca $ \rectPtr -> do
      poke rectPtr rect
      let cPtr = castPtr rectPtr
      func cPtr

instance FromPtr Rectangle where
  fromPtr ptr = do
    let rectPtr = castPtr ptr
    peek rectPtr

instance ToJSON Rectangle where
  toJSON (Rectangle left top right bottom) = object [ "left" .= (fromIntegral left :: Int)
                                                    , "right" .= (fromIntegral right :: Int)
                                                    , "top" .= (fromIntegral top :: Int)
                                                    , "bottom" .= (fromIntegral bottom :: Int)
                                                    ]
  toEncoding (Rectangle left top right bottom) = pairs ( "left" .= (fromIntegral left :: Int)
                                                      <> "right" .= (fromIntegral right :: Int)
                                                      <> "top" .= (fromIntegral top :: Int)
                                                      <> "bottom" .= (fromIntegral bottom :: Int)
                                                       )

toCLong :: Int -> CLong
toCLong = fromIntegral

instance FromJSON Rectangle where
  parseJSON = withObject "Rectangle" $ \o -> Rectangle
    <$> (toCLong <$> o .: "left")
    <*> (toCLong <$> o .: "top")
    <*> (toCLong <$> o .: "right")
    <*> (toCLong <$> o .: "bottom")
