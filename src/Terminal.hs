module Terminal where

import System.Console.ANSI
import Control.Exception

import Screen
import Types

openTerminal :: ((Screen -> IO ()) -> IO a) -> IO a
openTerminal cont = do
    clearScreen
    setSGR [SetColor Foreground Vivid Green]
    hideCursor
    let upd i = do
            clearScreen
            putStr $ unlines
                [ concat [ if pixAt i x y then "★★" else "  "
                | x <- [0..wIDTH-1]] | y <- [0..hEIGHT-1] ]
    cont upd `finally` (showCursor >> setSGR [Reset])


