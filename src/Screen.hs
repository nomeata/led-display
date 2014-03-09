module Screen where

import Codec.Picture
import qualified Data.Map as M

import Types

-- The type of an LED screen

-- 0 == off, anything else == 1
type Screen = Image Pixel8

emptyScreen :: Screen
emptyScreen = blankScreen (wIDTH, hEIGHT)

pixAt :: Screen -> Int -> Int -> Bool
pixAt s x y = pixelAt s x y == 0

pixAt' :: Screen -> Int -> Int -> Bool
pixAt' s x y = x >= 0 && y >= 0 &&
               x < imageWidth s && y < imageHeight s &&
               pixAt s x y

resize :: Rect -> Screen -> Screen
resize r i | r == (iw, ih) = i
           | otherwise     = generateScreen r $ pixAt' i
  where iw = imageWidth i
        ih = imageHeight i

cut :: Rect -> Rect -> Screen -> Screen
cut (x0,y0) (w,h) i = generateScreen (w,h) $ \x y -> pixAt' i (x+x0) (y+y0)



-- The type of a screen element

type Rect = (Int, Int)
type FrameCounter = Int
type Elem = Image Pixel8

generateScreen :: Rect -> (Int -> Int -> Bool) -> Screen
generateScreen (w,h) f = generateImage (\x y -> if f x y then 0 else 0xff) w h

blankScreen :: Rect -> Screen
blankScreen r = generateScreen r (\ _ _ -> False)


data ScreenElement = ScreenElement
    { seSize :: Rect
    , seRender :: Rect -> FrameCounter -> Elem
    }

toElem :: Screen -> ScreenElement
toElem i = ScreenElement (imageWidth i, imageHeight i) $ \r _ -> resize r i

padElem :: Rect -> ScreenElement
padElem r = ScreenElement r (\r' _ -> generateScreen r' (\_ _ -> False))

padX x = padElem (x,0)
padY y = padElem (0,y)

center :: ScreenElement -> ScreenElement
center se = ScreenElement (seSize se) $ \(w',h') f ->
    let i = seRender se (w,h) f
    in if (w,h) == (w',h')
       then i
       else generateScreen (w',h') $ \x y -> pixAt' i (x - (w'-w)`div`2) (y - (h'-h)`div`2)
  where (w,h) = seSize se

above :: ScreenElement -> ScreenElement -> ScreenElement
above se1 se2 = ScreenElement (max w1 w2, h1 + h2) $ \(w,h) f ->
    let h2 = h `div` 2
        i1 = seRender se1 (w, h2) f
        i2 = seRender se2 (w, h - h2) f
    in generateScreen (w,h) $ \x y ->
        if y < h2
        then pixAt i1 x y
        else pixAt i2 x (y - h2)
  where (w1,h1) = seSize se1
        (w2,h2) = seSize se2

besides :: ScreenElement -> ScreenElement -> ScreenElement
besides se1 se2 = ScreenElement (w1 + w2, max h1 h2) $ \(w,h) f ->
    let w2 = w `div` 2
        i1 = seRender se1 (w2, h) f
        i2 = seRender se2 (w - w2, h) f
    in generateScreen (w,h) $ \x y ->
        if x < w2
        then pixAt i1 x y
        else pixAt i2 (x-w2) y
  where (w1,h1) = seSize se1
        (w2,h2) = seSize se2

andElem :: ScreenElement -> ScreenElement -> ScreenElement
andElem se1 se2 = ScreenElement (max w1 w2, max h1 h2) $ \r f ->
    let i1 = seRender se1 r f
        i2 = seRender se2 r f
    in generateScreen r $ \x y -> pixAt i1 x y || pixAt i2 x y
  where (w1,h1) = seSize se1
        (w2,h2) = seSize se2

hconcat :: [ScreenElement] -> ScreenElement
hconcat elems = ScreenElement (w, h) $ \(w,h) f ->
    let is = map (\e -> seRender e (fst (seSize e), h) f) elems
    in generateScreen (w,h) $ \x y -> go x y is
  where w = sum $ map (fst . seSize) elems
        h = maximum $ 0 : map (snd . seSize) elems
        go x y [] = False
        go x y (i:is) | x < imageWidth i = pixAt i x y
                      | otherwise = go (x - imageWidth i) y is

scrollH :: ScreenElement -> ScreenElement
scrollH se = ScreenElement (w,h) $ \(w',h') f ->
    let cycleTime = w' + w
        i = seRender se (w, h) f
    in generateScreen (w',h') (\x y -> pixAt' i ((x - w' + f `div` slowness) `mod` cycleTime) y)
  where
    (w,h) = seSize se
    slowness = 2

spaceOut :: [ScreenElement] -> ScreenElement
spaceOut elems = ScreenElement (w, h) $ \(w',h') f ->
    let extraPixels = max 0 (w' - w)
        -- TO improve:
        holes = length elems - 1
        pad1 = extraPixels `div` holes 
        pad2 = pad1 + 1
        pad2n = extraPixels - holes * pad1
        pads = replicate (holes - pad2n) pad1 `zigZag` replicate pad2n pad2
        ScreenElement _ r = hconcat (elems `zigZag` map padX pads) 
    in r (w',h') f
  where
    w = sum $ map (fst . seSize) elems
    h = maximum $ 0 : map (snd . seSize) elems

zigZag :: [a] -> [a] -> [a]
zigZag [] ys = ys
zigZag (x:xs) ys = x : zigZag ys xs

alternate :: [ScreenElement] -> ScreenElement
alternate [] = toElem $ blankScreen (0,0)
alternate elems = ScreenElement (w,h) $ \(w,h) f ->
    seRender (elems !! ((f `div` duration) `mod` length elems)) (w,h) f
  where w = maximum $ map (fst . seSize) elems
        h = maximum $ map (snd . seSize) elems
        duration = 5 * fPS

alternateScroll :: Transition -> [ScreenElement] -> ScreenElement
alternateScroll tran [] = toElem $ blankScreen (0,0)
alternateScroll tran elems = ScreenElement (w,h) $ \(w,h) f ->
    let i1  = seRender (elems !! ( (f `div` duration)      `mod` length elems)) (w,h) f
        i2 = seRender (elems !! (((f `div` duration) + 1) `mod` length elems)) (w,h) f
        d = 1 - min 1 (fromIntegral (duration - (f `mod` duration)) / fromIntegral transitionTime)
    in  tran i1 i2 d
  where w = maximum $ map (fst . seSize) elems
        h = maximum $ map (snd . seSize) elems
        duration = 5 * fPS
        transitionTime = fPS

type Transition = Screen -> Screen -> Double -> Screen

dirac :: Transition
dirac i _ _ = i

scrollUp :: Transition
scrollUp i1 i2 d =
    generateScreen (w,h) $ \x y -> pixAt' i1  x (y+dy) || pixAt' i2 x (y+dy-h)
  where
    w = imageWidth i1
    h = imageHeight i1
    dy = round (d * fromIntegral h)

rollUp :: Transition
rollUp i1 i2 d =
    generateScreen (w,h) $
        \x y -> if y < dy then pixAt' i1 x y
                          else pixAt' i2 x y
  where
    w = imageWidth i1
    h = imageHeight i1
    dy = round ((1-d) * fromIntegral h)

transitions :: M.Map String Transition
transitions = M.fromList
    [ ("dirac", dirac)
    , ("scrollup", scrollUp)
    , ("rollup", rollUp)
    ]

