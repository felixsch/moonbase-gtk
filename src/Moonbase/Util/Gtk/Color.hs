module Moonbase.Util.Gtk.Color
    ( HexColor
    , parseColor
    , parseColor'
    ) where

import Numeric (readHex)

import Data.Char (isHexDigit)
import Graphics.UI.Gtk


type HexColor = String

parseColor :: HexColor -> Color
parseColor ['#',r,g,b] 
    = parseColor ['#',r,r,g,g,b,b]
parseColor s@['#',r1,r2,g1,g2,b1,b2]
  | all isHexDigit (tail s) = Color (gtkCol [r1,r2]) (gtkCol [g1,g2]) (gtkCol [b1,b2])
  | otherwise = parseColor "#000"
  where gtkCol x = 257 * fst (head $ readHex x)
parseColor _ = parseColor "#000"


parseColor' :: HexColor -> (Double, Double, Double)
parseColor' ['#', r, g, b] = parseColor' ['#', r, r, b, b, g, g]
parseColor' s@['#', r1, r2, g1, g2, b1, b2]
  | all isHexDigit (tail s) = (hexify [r1,r2], hexify [g1, g2], hexify [b1, b2])
  | otherwise               = parseColor' "#000000"
  where hexify = fst . head . readHex
parseColor' _              = parseColor' "#000000"
