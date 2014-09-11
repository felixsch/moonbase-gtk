module Moonbase.Util.Color
    ( parseColor
    , pangoColor
    , pangoSanitize
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


pangoColor :: String -> String -> String
pangoColor fg str = left ++ str ++ right
    where
        left = "<span foreground=\"" ++ fg ++ "\">"
        right = "</span>" 

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>' xs = "&gt;" ++ xs
    sanitize '<' xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&' xs = "&amp;" ++ xs
    sanitize x xs = x:xs
