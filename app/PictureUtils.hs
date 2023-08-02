module PictureUtils where

import Graphics.Gloss (BitmapData (bitmapSize), Picture (..), Point, scale, translate)
import Graphics.Gloss.Data.Bitmap (Rectangle (Rectangle))

translateCentered :: Float -> Float -> Picture -> Picture
translateCentered x y p = translate (x - w / 2) (y - h / 2) p
  where
    (w, h) = pictureSize p

drawPicture :: Picture -> Picture
drawPicture p = translate (fst s / 2) (snd s / 2) p
  where
    s = pictureSize p

drawPictureWithSize :: (Float, Float) -> Picture -> Picture
drawPictureWithSize (w, h) p = scale (w / fst s) (h / snd s) (drawPicture p)
  where
    s = pictureSize p

drawPictureWithMaxSize :: Float -> Picture -> Picture
drawPictureWithMaxSize s p = translate ((s - scaledWidth)/2) ((s-scaledHeight)/2) $ drawPictureWithSize (scaledWidth, scaledHeight) p
    where
        (width, height) = pictureSize p
        scaleFactor = s / max width height
        scaledWidth = width * scaleFactor
        scaledHeight = height * scaleFactor


getBmpDataFromPicture :: Picture -> Maybe BitmapData
getBmpDataFromPicture (Translate _ _ p) = getBmpDataFromPicture p -- Modifications to the image like color and translation have the picture layered in it like an onion.
getBmpDataFromPicture (Color _ p) = getBmpDataFromPicture p
getBmpDataFromPicture (Rotate _ p) = getBmpDataFromPicture p
getBmpDataFromPicture (Scale _ _ p) = getBmpDataFromPicture p
getBmpDataFromPicture (BitmapSection _ d) = Just d
getBmpDataFromPicture (Bitmap bdata) = Just bdata
getBmpDataFromPicture _ = Nothing

pictureSize :: Picture -> (Float, Float)
pictureSize (Bitmap bdata) = toFloatPair (bitmapSize bdata)
  where
    toFloatPair (a, b) = (fromIntegral a, fromIntegral b)
pictureSize (Circle r) = (r * 2, r * 2)
pictureSize (ThickCircle r s) = (s + r * 2, s + r * 2)
pictureSize (Color _ p) = pictureSize p -- Modifications to the image like color and translation have the picture layered in it like an onion.
pictureSize (Translate _ _ p) = pictureSize p
pictureSize (Rotate _ p) = pictureSize p -- TODO: Take into account the rotation, if a long box is rotated it's width and height may change
pictureSize (Scale x y p) = case pictureSize p of
  (x1, y1) -> (x1 * x, y1 * y)
pictureSize (Pictures ps) = maximum $ map pictureSize ps
pictureSize (BitmapSection (Rectangle _ (width, height)) _) = (fromIntegral width, fromIntegral height)
pictureSize (Polygon path) = elementWiseMap [elementWiseMap path max, elementWiseMap path min] (-)
  where
    elementWiseMap :: [Point] -> (Float -> Float -> Float) -> Point
    elementWiseMap [(x, y)] _ = (x, y)
    elementWiseMap ((x, y) : ps) f = case elementWiseMap ps f of
      (x1, y1) -> (f x x1, f y y1)
    elementWiseMap [] _ = error "Empty list in elementWiseMap"
pictureSize p = error ("Requested pictureSize for unkown shape " ++ show p) -- It should always have a pictureSize. If we don't know one it is a fatal error.
