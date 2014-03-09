module LedXML where

import Text.XML.Light
import Data.Functor
import qualified Data.Map as M
import Data.Char
import Data.Maybe
import System.FilePath

import Screen
import ScreenFile
import Sinus
import Fonts

-- XML file parsing

readXMLFile :: Font -> FilePath -> IO ScreenElement
readXMLFile font path = do
    xml <- readFile path
    let Just root = parseXMLDoc xml
    xmlToScreenElement font root


xmlToScreenElement :: Font -> Element -> IO ScreenElement
xmlToScreenElement f e
  | n == "screen"  = hconcat <$> ch
  | n == "scrollH" = scrollH <$> hconcat <$> ch
  | n == "spaceout" = spaceOut <$> ch
  | n == "center"  = center <$> hconcat <$> ch
  | n == "alternate", Just t <- findAttr (unqual "transition") e
                     = let trans = fromMaybe (error $ "Unknown transition " ++ show t) $
                                   M.lookup (map toLower t) transitions
                       in alternateScroll trans <$> ch
  | n == "alternate" = alternate <$> ch
  | n == "sinus"   = return sinusScreenElement
  | n == "icon"    = do
        let basename = strContent e
        readFromGif $ "icons" </> basename ++ ".gif"
  | n == "thintext" = return $ text thinfont (strContent e)
  | n == "text"     = return $ text f (strContent e)
  | otherwise = error $ "Unknown element  " ++ show e

  where
    n = qName (elName e)
    ch = mapM (xmlToScreenElement f) (elChildren e)


