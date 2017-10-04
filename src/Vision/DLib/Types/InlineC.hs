
{-# LANGUAGE CPP             #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Vision.DLib.Types.InlineC
Description : Inline-C utilities
Copyright   : (c) Jordan Medlock, 2017
Maintainer  : jordanemedlock@gmail.com
Portability : POSIX

Containes the dlibCtx context value.
-}
module Vision.DLib.Types.InlineC where

import qualified Data.ByteString.Unsafe              as BS
import qualified Data.Map                            as M
import           Data.Monoid                         (mempty, (<>))
import           Foreign.C.Types
import           Foreign.Ptr
import qualified Language.C.Inline                   as C
import qualified Language.C.Inline.Context           as C
import qualified Language.C.Inline.Cpp               as C
import qualified Language.C.Inline.HaskellIdentifier as C
import qualified Language.C.Types                    as C
import qualified Language.Haskell.TH                 as TH
import           Vision.DLib.Types.C



-- | Custom dlib inline-c context containing dlib types
dlibCtx :: C.Context
dlibCtx = C.cppCtx <> C.bsCtx <> C.vecCtx <> C.funCtx <> ctx
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
  , ( C.TypeName "shape_predictor", [t| C'ShapePredictor |])
  , ( C.TypeName "frontal_face_detector", [t| C'FrontalFaceDetector |])
  , ( C.TypeName "rgb_pixel", [t| C'RGBPixel |])
  ]


getHsVariable :: String -> C.HaskellIdentifier -> TH.ExpQ
getHsVariable err s = do
  mbHsName <- TH.lookupValueName $ C.unHaskellIdentifier s
  case mbHsName of
    Nothing -> fail $ "Cannot capture Haskell variable " ++ C.unHaskellIdentifier s ++
                      ", because it's not in scope. (" ++ err ++ ")"
    Just hsName -> TH.varE hsName


strAntiQuoter :: C.AntiQuoter C.HaskellIdentifier
strAntiQuoter = C.AntiQuoter
  { C.aqParser = do
      hId <- C.parseIdentifier
      let cId = C.mangleHaskellIdentifier hId
      return (cId, C.Ptr [] (C.TypeSpecifier mempty (C.Char Nothing)), hId)
  , C.aqMarshaller = \_purity _cTypes cTy cId -> do
      case cTy of
        C.Ptr _ (C.TypeSpecifier _ (C.Char Nothing)) -> do
          hsTy <- [t| Ptr CChar |]
          hsExp <- getHsVariable "strCtx" cId
          hsExp' <- [| \cont -> BS.unsafeUseAsCString $(return hsExp) $ \ptr -> cont ptr |]
          return (hsTy, hsExp')
        _ -> fail "impossible: got type different from `char *` (strCtx)"
  }

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
