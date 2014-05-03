module Moonbase.Util.Color
    ( parseColor
    ) where

import Numeric (readHex)

import Data.Char (isHexDigit)
import Graphics.UI.Gtk


parseColor :: String -> Color
parseColor ['#',r,g,b] 
    = parseColor ['#',r,r,g,g,b,b]
parseColor s@['#',r1,r2,g1,g2,b1,b2]
  | all isHexDigit (tail s) = Color (gtkCol [r1,r2]) (gtkCol [g1,g2]) (gtkCol [b1,b2])
  | otherwise = parseColor "#000"
  where gtkCol x = 257 * fst (head $ readHex x)
parseColor _ = parseColor "#000"
