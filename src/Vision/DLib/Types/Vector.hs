{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Vision.DLib.Types.Vector where


import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import GHC.TypeLits
import Vision.DLib.Types.C
import Vision.DLib.Types.InlineC

C.context dlibCtx

data Point = Point 
  { ptX :: CLong
  , ptY :: CLong
  } deriving Show

type instance C Point = C'Point

alloca_point f = do
  ptr <- [C.exp| point * { new point() }|]
  
  ret <- f ptr
  
  [C.block| void { delete $(point * ptr); }|]
  
  return ret

instance WithPtr Point where
  withPtr (Point x y) func = do
    alloca_point $ \ptr -> do
      [C.block| void {
        $(point * ptr)->x = $(long x);         
        $(point * ptr)->y = $(long y); 
      }|]
      func ptr

instance FromPtr Point where 
  fromPtr ptr = do
    x <- [C.exp| long { $(point * ptr)->x }|]
    y <- [C.exp| long { $(point * ptr)->y }|]
    return $ Point x y

