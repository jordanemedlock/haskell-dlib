module Vision.DLib.Types.RGBPixel where


import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr


data RGBPixel = RGBPixel
  { rgbRed :: CChar
  , rgbGreen :: CChar
  , rgbBlue :: CChar
  } deriving (Show)

instance Storable RGBPixel where
  sizeOf _ = 3
  alignment = sizeOf
  peek ptr = do
    let charPtr = castPtr ptr
    red <- peekElemOff charPtr 0
    green <- peekElemOff charPtr 0
    blue <- peekElemOff charPtr 0
    return $ RGBPixel red green blue
  poke ptr (RGBPixel r g b) = do
    let charPtr = castPtr ptr
    pokeElemOff charPtr 0 r
    pokeElemOff charPtr 1 g
    pokeElemOff charPtr 2 b
