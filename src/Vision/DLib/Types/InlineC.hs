
{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

{-|
Module      : Vision.DLib.Types.InlineC
Description : Inline-C utilities
Copyright   : (c) Jordan Medlock, 2017
Maintainer  : jordanemedlock@gmail.com
Portability : POSIX

Containes the dlibCtx context value.
-}
module Vision.DLib.Types.InlineC where


import Foreign.Ptr ( FunPtr )
import Data.Monoid ( (<>), mempty )
import qualified Data.Map as M
import qualified Language.C.Inline as C
import qualified Language.C.Types  as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as C
import Vision.DLib.Types.C

-- | Custom dlib inline-c context containing dlib types
dlibCtx :: C.Context
dlibCtx = C.cppCtx <> C.bsCtx <> C.vecCtx <> ctx
 where ctx = mempty { C.ctxTypesTable = dlibTypesTable }

-- | DLib types table between C++ type and Haskell-C type
dlibTypesTable :: C.TypesTable
dlibTypesTable = M.fromList
  [ ( C.TypeName "bool", [t| C.CInt |] )
  , ( C.TypeName "rectangle", [t| C'Rectangle |] )
  , ( C.TypeName "point", [t| C'Point |] )
  , ( C.TypeName "full_object_detection", [t| C'Shape |])
  , ( C.TypeName "image_window", [t| C'ImageWindow |])
  , ( C.TypeName "image", [t| C'Image |])
  ]

{-
cppVecCtx :: C.Context
cppVecCtx = mempty
  { C.ctxAntiQuoters = Map.fromList
    [ ( "cpp-vec", SomeAntiQuoter cppVecAntiQuoter)
    ]
  }

cppVecAntiQuoter :: AntiQuoter HaskellIdentifier
cppVecAntiQuoter = AntiQuoter
  { aqParser = cDeclAqParser
  , aqMarshaller = \purity cTypes cTy cId -> do
      hsTy <- convertType_ "vecCtx" purity cTypes cTy
      hsExp <- getHsVariable "vecCtx" cId
      hsExp' <- [| vecCtxUnsafeWith $(return hsExp) |]
      return (hsTy, hsExp')
  }
  -}
