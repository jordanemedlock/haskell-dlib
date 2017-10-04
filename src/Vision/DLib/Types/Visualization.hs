
module Vision.DLib.Types.Visualization where

data Stroke = Stroke { strokeColor :: RGBColor, strokeWidth :: Int }
newtype Fill = Fill { fillColor :: RGBColor }

data Visualization = VisRect { visRect :: Rectangle,                      visFill :: Maybe Fill, visStroke :: Maybe Stroke }
                   | VisLine { visLine :: (Point, Point),                 visFill :: Maybe Fill, visStroke :: Maybe Stroke }
                   | VisCircle { visCenter :: Point, visRadius :: Double, visFill :: Maybe Fill, visStroke :: Maybe Stroke }


drawVis :: Image -> Visualization -> IO Image
drawVis (Image img) (VisRect r Nothing (Just (Stroke c w))) = withPtr r $ \rPtr -> withPtr c $ \cPtr -> do
  let w' = fromIntegral w
  [C.block| void {
    draw_rectangle($(image * img), $(rectangle * rPtr), $(rgb_pixel * cPtr), $(int w'));
  } |]
  return img
drawVis (Image img) (VisRect r (Just (Fill c))) = withPtr r $ \rPtr -> withPtr c $ \cPtr -> do
  [C.block| void {
    fill_rect($(image * img), $(rectangle * rect), $(rgb_pixel * cPtr));
  } |]
  return img
drawVis img (VisRect r f@(Just _) s@(Just _)) = drawVis img (VisRect r f Nothing) >> drawVis img (VisRect r Nothing s)
drawVis img (VisLine (p1,p2) (Just (Fill c)) Nothing) = error "Not implemented yet"
