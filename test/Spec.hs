{-# LANGUAGE CPP #-}

import           Control.Monad
import           Foreign.Ptr
import qualified Language.C.Inline as C
import           System.Directory
import           System.Exit
import           Vision.DLib

-- C.context dlibCtx
--
-- C.include "<iostream>"
-- C.include "<dlib/geometry.h>"
--
-- C.using "namespace std"
-- C.using "namespace dlib"

assertIO :: String -> Bool -> IO ()
assertIO _ True    = return ()
assertIO msg False = putStrLn msg >> exitFailure

assertEquals :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEquals msg x y = assertIO (msg ++ show x ++ " is: " ++ show y) (x == y)

assertNotEquals :: (Eq a, Show a) => String -> a -> a -> IO ()
assertNotEquals msg x y = assertIO (msg ++ show x ++ " is: " ++ show y) (x /= y)

assertNull :: String -> Ptr a -> IO ()
assertNull msg = assertEquals msg nullPtr

assertNotNull :: String -> Ptr a -> IO ()
assertNotNull msg = assertNotEquals msg nullPtr

assertImageExistsWith :: Image -> (C.CULong, C.CULong) -> IO ()
assertImageExistsWith img@(Image ptr) (width, height) = do
  assertNotNull "image should not be " ptr
  assertEquals "image width should be " width $ imgWidth img
  assertEquals "image height should be " height $ imgHeight img


main :: IO ()
main = do
  testImage
  testLoadImage
  testSaveImage
  testPyramidUp
  testFrontalFaceDetector

testImage :: IO ()
testImage = do
  img <- mkImage
  assertImageExistsWith img (0,0)
  destroyImage img

monaLisa :: String
monaLisa = "data/mona_lisa.png"
testImg :: String
testImg = "data/test_img.png"

testLoadImage :: IO ()
testLoadImage = do
  img <- mkImage
  loadImage img monaLisa
  assertImageExistsWith img (240,359)
  destroyImage img

testSaveImage :: IO ()
testSaveImage = do
  img <- mkImage
  loadImage img monaLisa
  b <- doesFileExist testImg
  when b $ removeFile testImg
  saveImage img testImg
  assertIO "test image should exist" =<< doesFileExist testImg
  assertEquals "file size should be " 132189 =<< getFileSize testImg
  destroyImage img

testPyramidUp :: IO ()
testPyramidUp = do
  img <- mkImage
  loadImage img monaLisa
  _ <- pyramidUp img
  assertImageExistsWith img (482,719)
  destroyImage img

testFrontalFaceDetector :: IO ()
testFrontalFaceDetector = do
  img <- mkImage
  det <- mkFrontalFaceDetector

  loadImage img monaLisa
  rects <- runFrontalFaceDetector det img

  assertEquals "first rect should be " (Rectangle 77 61 149 133) (head rects)

  destroyImage img
  destroyFrontalFaceDetector det
