{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

import Vision.DLib
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

C.context dlibCtx

C.include "<iostream>"
C.include "<dlib/geometry.h>"

C.using "namespace std"
C.using "namespace dlib"

main :: IO ()
main = do
  testRect

testRect :: IO ()
testRect = do
  putStrLn ""
  let rect = Rectangle 10 20 30 40
  withPtr rect $ \rectPtr -> do
    [C.block| void {
      cout << "internal c++ rectangle " << *$(rectangle * rectPtr) << endl;
    }|]
    newRect <- fromPtr rectPtr
    putStrLn $ (show rect) ++ " == " ++ (show newRect) ++ " => " ++ (show $ newRect == rect)
