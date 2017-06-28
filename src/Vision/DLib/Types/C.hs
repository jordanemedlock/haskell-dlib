
{-|
Module      : Vision.DLib.Types.C
Description : C utilities and types
Copyright   : (c) Jordan Medlock, 2017
Maintainer  : jordanemedlock@gmail.com
Portability : POSIX

Contains many C utilities and types.
-}
module Vision.DLib.Types.C where


import Foreign.C.Types ( CLong, CDouble )
import Foreign.Ptr ( Ptr )
import GHC.TypeLits ( Nat ) 

-- | Type family of C representations of Haskell types.
type family C (a :: *) :: *

class CSizeOf a where
  -- | Computes the storage requirements (in bytes) of values of
  -- type @a@ in C.
  cSizeOf :: a -> Int
  cAlignOf :: a -> Int

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
  fromPtr :: Ptr (C a) -> IO a

--objFromPtr :: (ForeignPtr c -> hask) -> (Ptr c -> IO ()) -> IO (Ptr c) -> IO hask
--objFromPtr haskCons finalizer mkObjPtr = mask_ $ do
--  objPtr <- mkObjPtr
--  haskCons <$> newForeignPtr objPtr (finalizer objPtr)

-- | Represents the dlib type @vector@
data C'Vector (depth :: *) (numR :: Nat)

-- | Represents the dlib type @point@
type C'Point = C'Vector CLong 2

-- | Represents the dlib type @dpoint@
type C'DPoint = C'Vector CDouble 2

-- | Represents the dlib type @rectangle@
data C'Rectangle

-- | Represents the dlib type @full_object_detection@
data C'Shape

-- | Represents the dlib type @image_window@
data C'ImageWindow

-- | Represents the dlib type @array_2d@
data C'Array2D (depth :: *)

-- | Represents the dlib type @rgb_pixel@
data C'RGBPixel

-- | Represents the dlib type @array_2d<rgb_pixel>@
type C'Image = C'Array2D C'RGBPixel
