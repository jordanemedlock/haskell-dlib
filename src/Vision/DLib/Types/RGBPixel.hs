module Vision.DLib.Types.RGBPixel where


import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Vision.DLib.Types.C

red :: RGBPixel
red = RGBPixel 255 0 0

orange :: RGBPixel
orange = RGBPixel 255 100 0

yellow :: RGBPixel
yellow = RGBPixel 255 255 0

green :: RGBPixel
green = RGBPixel 0 255 0

cyan :: RGBPixel
cyan = RGBPixel 0 255 255

blue :: RGBPixel
blue = RGBPixel 0 0 255

purple :: RGBPixel
purple = RGBPixel 255 0 255

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
    r <- peekElemOff charPtr 0
    g <- peekElemOff charPtr 0
    b <- peekElemOff charPtr 0
    return $ RGBPixel r g b
  poke ptr (RGBPixel r g b) = do
    let charPtr = castPtr ptr
    pokeElemOff charPtr 0 r
    pokeElemOff charPtr 1 g
    pokeElemOff charPtr 2 b

type instance C RGBPixel = C'RGBPixel

instance WithPtr RGBPixel where
  withPtr pix func = do
    alloca $ \pixPtr -> do
      poke pixPtr pix
      let cPtr = castPtr pixPtr
      func cPtr

instance FromPtr RGBPixel where
  fromPtr ptr = do
    let pixPtr = castPtr ptr
    peek pixPtr
