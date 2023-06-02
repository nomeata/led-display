module USB where

import Codec.Picture
import System.USB
import qualified Data.ByteString as B
import Data.Bits
import Data.IORef
import Control.Concurrent
import Control.Monad
import qualified Data.Vector as V
import System.Exit


import Screen
import Types

-- USB interface

toPkg :: Screen -> [B.ByteString]
toPkg s =
    [ B.pack $ 0 : fromIntegral r : row r ++ row (r+1)
    | r <- [0,2,4,6]]
  where
    row r = [ byte r o | o <- [16,8,0] ]
    byte r o = sum [ bit i | r < hEIGHT, i <- [0..7], o + i < wIDTH, pixelAt s (o+i) r > 0]


openLEDDevice :: ((Screen -> IO ()) -> IO a) -> IO a
openLEDDevice cont = do
    ref <- newIORef emptyScreen
    forkIO $ do
        ctx <- newCtx
        setDebug ctx PrintWarnings
        devs <- getDevices ctx
        mbdev <- findM isLEDDisplay devs
        case mbdev of
            Nothing -> do
                putStrLn "Device 0x1d34:0013 not found."
                putStrLn "Devices present:"
                V.mapM (putStrLn . ("    "++) . show) devs
                exitFailure
            Just dev -> withDeviceHandle dev $ \h -> forever $ do
                    s <- readIORef ref
                    forM_ (toPkg s) $ \p -> do
                        writeControl h (ControlSetup Class ToEndpoint 0x09 0 0) p 1000
                    threadDelay fRAME_DELAY
    cont (writeIORef ref)

isLEDDisplay :: Device -> IO Bool
isLEDDisplay dev = do
    desc <- getDeviceDesc dev
    return $     deviceVendorId desc  == 0x1d34
             &&  deviceProductId desc == 0x0013


findM :: Monad m => (a -> m Bool) -> V.Vector a -> m (Maybe a)
findM pred = go . V.toList
  where go [] = return Nothing
        go (x:xs) = do
            p <- pred x
            if p then return (Just x) else go xs
