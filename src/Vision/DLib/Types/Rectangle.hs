module Vision.DLib.Types.Rectangle where

import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Vision.DLib.Types.C

data Rectangle = Rectangle 
  { rectLeft :: CLong
  , rectTop :: CLong
  , rectRight :: CLong
  , rectBottom :: CLong
  } deriving (Show)
                    
type instance C Rectangle = C'Rectangle
                           
instance Storable Rectangle where
  sizeOf _ = (sizeOf (0 :: CLong)) * 4
  alignment _ = 1
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