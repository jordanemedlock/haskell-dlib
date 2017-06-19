module Vision.DLib.Types.C where

import Foreign.C.Types
import Foreign.Ptr ( Ptr, nullPtr )
import Data.Int ( Int32 )
import GHC.TypeLits

type family C (a :: *) :: *

class CSizeOf a where
  -- | Computes the storage requirements (in bytes) of values of
  -- type @a@ in C.
  cSizeOf :: proxy a -> Int
  
-- | Perform an IO action with a pointer to the C equivalent of a value
class WithPtr a where
  -- | Perform an action with a temporary pointer to the underlying
  -- representation of @a@
  --
  -- The pointer is not guaranteed to be usuable outside the scope of this
  -- function. The same warnings apply as for 'withForeignPtr'.
  withPtr :: a -> (Ptr (C a) -> IO b) -> IO b


-- | Types of which a value can be constructed from a pointer to the C
-- equivalent of that value
--
-- Used to wrap values created in C.
class FromPtr a where
  fromPtr :: IO (Ptr (C a)) -> IO a

objFromPtr :: (ForeignPtr c -> hask) -> (Ptr c -> IO ()) -> IO (Ptr c) -> IO hask
objFromPtr haskCons finalizer mkObjPtr = mask_ $ do
  objPtr <- mkObjPtr
  haskCons <$> newForeignPtr objPtr (finalizer objPtr)


data C'Vector (depth :: *) (numR :: Nat)

type C'Point = C'Vector CLong 2
type C'DPoint = C'Vector CDouble 2

data C'Rectangle