{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}


{-|
Module      : Vision.DLib.Types.Constants
Description : Type Constants
Copyright   : (c) Jordan Medlock, 2017
Maintainer  : jordanemedlock@gmail.com
Portability : POSIX
-}
module Vision.DLib.Types.Constants where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Data.Aeson
import Data.Monoid

C.context C.cppCtx
C.include "<stdlib>"
C.include "<dlib/geometry.h>"
C.using "namespace std"
C.using "namespace dlib"

sizeofRect :: Int
sizeofRect = fromIntegral [C.pure| long { sizeof(dlib::rectangle) }|]
alignofRect :: Int
alignofRect = fromIntegral [C.pure| long { alignof(dlib::rectangle) }|]