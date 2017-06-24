module Vision.DLib.Types.Rectangle
( Rectangle(..)
) where

import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Vision.DLib.Types.C
import Vision.DLib.Types.Constants
import Data.Aeson
import Data.Monoid

data Rectangle = Rectangle
  { rectLeft :: CLong
  , rectTop :: CLong
  , rectRight :: CLong
  , rectBottom :: CLong
  } deriving (Show)

type instance C Rectangle = C'Rectangle

instance Storable Rectangle where
  sizeOf _ = fromIntegral sizeofRect
  alignment _ = fromIntegral alignofRect
  peek ptr = do
    let longPtr = castPtr ptr :: Ptr CLong
    l <- peekElemOff longPtr 0
    t <- peekElemOff longPtr 1
    r <- peekElemOff longPtr 2
    b <- peekElemOff longPtr 3
    return $ Rectangle l r t b

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
  parseJSON (Object o) = Rectangle
    <$> (toCLong <$> o .: "left")
    <*> (toCLong <$> o .: "top")
    <*> (toCLong <$> o .: "right")
    <*> (toCLong <$> o .: "bottom")
