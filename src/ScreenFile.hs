module ScreenFile where

import Codec.Picture
import Data.Functor

import Screen

readFromGif :: FilePath -> IO ScreenElement
readFromGif filename = do
    gif <- either error id <$> readGifImages filename
    let elems = map (pixelMap (\ (PixelRGB8 r g b) -> maximum [r,g,b])) gif
    let n = length elems
    return $ ScreenElement (imageWidth (head elems), imageHeight (head elems)) $
        \r f -> resize r (elems !! (f `mod` n))


