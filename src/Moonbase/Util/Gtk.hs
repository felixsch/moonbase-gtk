module Moonbase.Util.Gtk 
 ( iosync
 , parseColor
 , pangoColor
 , pangoSanitize
 ) where

import Moonbase.Util.Gtk.Color


import Control.Monad.Except
import Graphics.UI.Gtk.General.General

 
iosync :: (MonadIO m) => IO a -> m a
iosync = liftIO . postGUISync


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

