
import Vision.DLib


main :: IO ()
main = do
  testRect

testRect :: IO ()
testRect = do
  let rect = Rectangle 10 20 30 40
  print rect