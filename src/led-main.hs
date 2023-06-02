import qualified Data.Map as M
import System.Environment
import Control.Concurrent
import System.FSNotify
import System.FilePath
import Data.IORef

import Types
import Screen
import Fonts
import USB
import LedXML

--main = openTerminal $ \setScreen -> do
main = openLEDDevice $ \setScreen -> do
    font <- readFont
    [filename] <- getArgs
    elemRef <- reReadFile (readXMLFile font) filename

    renderLoop setScreen elemRef

reReadFile :: (FilePath -> IO a) -> FilePath -> IO (IORef a)
reReadFile read name = do
    -- let name' = FP.decodeString name
    x <- read name
    ref <- newIORef x
    manager <- startManager
    watchDir manager name
        (\e -> case e of Modified p _ _ -> p == name
                         Added p _ _    -> p == name
                         _              -> False)
        (\e -> read name >>= writeIORef ref)
    return ref


renderLoop :: (Elem -> IO a) -> IORef ScreenElement -> IO a
renderLoop setScreen elemRef = go 0
  where
    go n = do
        elem <- readIORef elemRef
        setScreen (seRender elem (wIDTH, hEIGHT) n)
        threadDelay fRAME_DELAY
        go (n+1)

