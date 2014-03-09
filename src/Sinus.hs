module Sinus where

import Screen

sinusScreenElement = ScreenElement (0,0) $ \(w,h) f ->
    generateScreen (w,h) $ \x y ->
        abs (fromIntegral y + 0.5  - (fromIntegral h/2)
                 - sin (fromIntegral (x + f) / 2) * (fromIntegral h/2)) <= (0.5::Double)
