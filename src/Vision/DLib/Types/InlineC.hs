
{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}


module Vision.DLib.Internal.InlineC where


import Foreign.Ptr ( FunPtr )
import Data.Monoid ( (<>) )
import qualified Data.Map as M
import qualified Language.C.Inline as C
import qualified Language.C.Types  as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as C


dlibCtx :: C.Context
dlibCtx = C.cppCtx <> C.bsCtx <> C.vecCtx <> ctx
 where ctx = memty { C.ctxTypesTable = dlibTypesTable }
 

dlibTypesTable :: C.TypesTable
dlibTypesTable = M.fromList 
  [ ( C.TypeName "bool", [t| C.CInt |] )
  , ( C.TypeName "rectangle", [t| C'Rectangle |] )
  ]