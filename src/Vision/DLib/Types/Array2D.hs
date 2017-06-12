module Vision.DLib.Types.Array2D where


import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Vision.DLib.Types.RGBPixel

newtype Image = Image (Ptr ())

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
