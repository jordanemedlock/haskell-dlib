{-# LANGUAGE TemplateHaskell #-}

module Vision.DLib.DNN.TH where


import Vision.DLib.DNN.Types
import qualified Language.C.Types as C
import Language.C.Inline.Internal
import Foreign.Ptr 
import Language.Haskell.TH as TH



deserialize :: HasCppType a => proxy a -> ExpQ
deserialize p = do
  funcName <- uniqueCName
  let typ = cppTypes (undefined :: proxy a)
  let code = Code 
        { codeCallSafety = TH.Safe
        , codeType = [t| String -> Ptr () -> IO () |]
        , codeFunName = cFuncName
        , codeDefs = "void " ++ cFuncName ++ "(string file, void * obj) {\n" ++ typ ++ "* typedObject = ("++typ++"*)obj;\ndeserialize(file) >> typedObject;}"
        }
  return $ inlineCode code






