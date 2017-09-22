
module Vision.DLib.Monad 
( DLib

, saveImage
, loadImage
, fromIplImage
, mkShapePredictor
, runShapePredictor
, mkFrontalFaceDetector
, runFrontalFaceDetector
, mkImageWindow
, clearOverlay
, addOverlay
, setImage
, mkImage
, pyramidUp

, Image
, Shape
, Rectangle
, ShapePredictor
, FrontalFaceDetector
, ImageWindow
, Overlay

, runDLib
) where

import           Vision.DLib.Types.Array2D (Image)
import qualified Vision.DLib.Types.Array2D as D
import           Vision.DLib.Types.Shape (Shape)
import           Vision.DLib.Types.Rectangle (Rectangle)
import           Vision.DLib.Algorithms.FeatureExtraction (ShapePredictor)
import qualified Vision.DLib.Algorithms.FeatureExtraction as D
import           Vision.DLib.Algorithms.ObjectDetection (FrontalFaceDetector)
import qualified Vision.DLib.Algorithms.ObjectDetection as D
import           Vision.DLib.GUI.ImageWindow (ImageWindow, Overlay)
import qualified Vision.DLib.GUI.ImageWindow as D
import qualified Vision.DLib.IO as D
import qualified Vision.DLib.OpenCV as D
import Foreign.Ptr
import Control.Monad.Free
import Control.Monad.IO.Class

data DLibF x = SaveImage String Image x
             | LoadImage String (Image -> x)
             | FromIplImage (Ptr ()) (Image -> x)
             | MakeShapePredictor String (ShapePredictor -> x)
             | RunShapePredictor ShapePredictor Image Rectangle (Shape -> x)
             | MakeFrontalFaceDetector (FrontalFaceDetector -> x)
             | RunFrontalFaceDetector FrontalFaceDetector Image ([Rectangle] -> x)
             | MakeImageWindow (ImageWindow -> x)
             | ClearOverlay ImageWindow (ImageWindow -> x)
             | SetImage ImageWindow Image (ImageWindow -> x)
             | AddOverlay ImageWindow Overlay (ImageWindow -> x)
             | MakeImage (Image -> x)
             | PyramidUp Image (Image -> x)
             | LiftIO (IO x)

instance Functor DLibF where
    fmap f (SaveImage str img x) = SaveImage str img (f x)
    fmap f (LoadImage str g) = LoadImage str (f . g)
    fmap f (FromIplImage ptr g) = FromIplImage ptr (f . g)
    fmap f (MakeShapePredictor str g) = MakeShapePredictor str (f . g)
    fmap f (RunShapePredictor sp img rect g) = RunShapePredictor sp img rect (f . g)
    fmap f (MakeFrontalFaceDetector g) = MakeFrontalFaceDetector (f . g)
    fmap f (RunFrontalFaceDetector det img g) = RunFrontalFaceDetector det img (f . g)
    fmap f (MakeImageWindow g) = MakeImageWindow (f . g)
    fmap f (ClearOverlay win g) = ClearOverlay win (f . g)
    fmap f (SetImage win img g) = SetImage win img (f . g)
    fmap f (AddOverlay win ovr g) = AddOverlay win ovr (f . g)
    fmap f (MakeImage g) = MakeImage (f . g)
    fmap f (PyramidUp img g) = PyramidUp img (f . g)
    fmap f (LiftIO g) = LiftIO (fmap f g)

type DLib = Free DLibF

instance MonadIO DLib where
    liftIO = liftF . LiftIO

saveImage :: String -> Image -> DLib ()
saveImage str img = liftF $ SaveImage str img ()

loadImage :: String -> DLib Image
loadImage str = liftF $ LoadImage str id

fromIplImage :: Ptr () -> DLib Image
fromIplImage ptr = liftF $ FromIplImage ptr id

mkShapePredictor :: String -> DLib ShapePredictor
mkShapePredictor str = liftF $ MakeShapePredictor str id

runShapePredictor :: ShapePredictor -> Image -> Rectangle -> DLib Shape
runShapePredictor sp img rect = liftF $ RunShapePredictor sp img rect id

mkFrontalFaceDetector :: DLib FrontalFaceDetector
mkFrontalFaceDetector = liftF $ MakeFrontalFaceDetector id

runFrontalFaceDetector :: FrontalFaceDetector -> Image -> DLib [Rectangle]
runFrontalFaceDetector det img = liftF $ RunFrontalFaceDetector det img id

mkImageWindow :: DLib ImageWindow
mkImageWindow = liftF $ MakeImageWindow id

clearOverlay :: ImageWindow -> DLib ImageWindow
clearOverlay win = liftF $ ClearOverlay win id

setImage :: ImageWindow -> Image -> DLib ImageWindow
setImage win img = liftF $ SetImage win img id

addOverlay :: ImageWindow -> Overlay -> DLib ImageWindow
addOverlay win ovr = liftF $ AddOverlay win ovr id

mkImage :: DLib Image
mkImage = liftF $ MakeImage id

pyramidUp :: Image -> DLib Image
pyramidUp img = liftF $ PyramidUp img id

runDLib :: DLib x -> IO x
runDLib (Pure x)                            = return x
runDLib (Free (SaveImage str img x))        = D.saveImage img str   >> runDLib x
runDLib (Free (LoadImage str f))            = do
    img <- D.mkImage
    D.loadImage img str       
    x <- runDLib (f img)
    D.destroyImage img
    return x
runDLib (Free (FromIplImage ptr f))         = D.fromIplImage ptr    >>= runDLib . f
runDLib (Free (MakeShapePredictor str f))   = do
    sp <- D.mkShapePredictor 
    D.deserializeShapePredictor sp str
    x <- runDLib (f sp)
    D.destroyShapePredictor sp
    return x
runDLib (Free (RunShapePredictor sp img rect f)) = D.runShapePredictor sp img rect >>= runDLib . f
runDLib (Free (MakeFrontalFaceDetector f)) = do
    det <- D.mkFrontalFaceDetector
    x <- runDLib (f det)
    D.destroyFrontalFaceDetector det
    return x
runDLib (Free (RunFrontalFaceDetector det img f)) = D.runFrontalFaceDetector det img >>= runDLib . f
runDLib (Free (MakeImageWindow f)) = do
    win <- D.mkImageWindow 
    x <- runDLib (f win)
    D.destroyImageWindow win
    return x
runDLib (Free (ClearOverlay win f)) = D.winClearOverlay win >> runDLib (f win)
runDLib (Free (SetImage win img f)) = D.winSetImage win img >> runDLib (f win)
runDLib (Free (AddOverlay win ovr f)) = D.winAddOverlay win ovr >> runDLib (f win)
runDLib (Free (MakeImage f)) = do
    img <- D.mkImage 
    x <- runDLib (f img)
    D.destroyImage img
    return x
runDLib (Free (PyramidUp img f)) = D.pyramidUp img >>= runDLib . f
runDLib (Free (LiftIO f)) = f >>= runDLib






