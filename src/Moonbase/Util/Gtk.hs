module Moonbase.Util.Gtk 
 ( iosync
 , parseColor
 , pangoColor
 , pangoSanitize
 , Position(..)
 , moveWindow
 , setWindowHints
 , setWindowStruts
 , getAbsoluteMousePosition
 ) where

import Moonbase.Util.Gtk.Color


import Control.Monad.Except
import Graphics.UI.Gtk
import Moonbase.Util.Gtk.StrutProperties


data Position = Top
  | Bottom
  | Custom Int

 
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


moveWindow :: Window -> Position -> Int -> Rectangle -> IO ()
moveWindow win pos height (Rectangle x _ _ h) = windowMove win x offset
    where
        offset = case pos of
                      Top            -> 0
                      Bottom         -> h - height
                      Custom height' -> h - height - height'

setWindowHints :: Window -> Rectangle -> Int -> IO ()
setWindowHints win (Rectangle _ _ w _) height = 
    windowSetGeometryHints win noWidget size size Nothing Nothing Nothing
      where
          noWidget = Nothing :: Maybe Widget
          size     = Just (w, height)


strutProperties :: Position -- ^ Bar position
                -> Int -- ^ Bar height
                -> Rectangle -- ^ Current monitor rectangle
                -> [Rectangle] -- ^ All monitors
                -> StrutProperties
strutProperties pos bh (Rectangle mX mY mW mH) monitors = propertize pos sX sW sH
    where
        sX = mX
        sW = mW - 1
        sH = case pos of
            Top -> bh + mY
            Bottom -> bh + totalH - mY - mH
        totalH = maximum $ map bottomY monitors
        bottomY (Rectangle _ y _ h) = y + h
        propertize p x w h = case p of
            Top    -> StrutProperties 0 0 h 0 0 0 0 0 x (x+w) 0 0
            Bottom -> StrutProperties 0 0 0 h 0 0 0 0 0 0 x (x+w)


setWindowStruts :: Window -> Position -> Int -> Rectangle -> IO ()
setWindowStruts win pos height geo = do
    scr    <- windowGetScreen win
    moNum  <- screenGetNMonitors scr
    moGeos <- mapM (screenGetMonitorGeometry scr) [0 .. (moNum - 1)]
    
    setStrutProperties win $ strutProperties pos height geo moGeos


getAbsoluteMousePosition :: Screen -> IO (Int, Int)
getAbsoluteMousePosition scr = do
    root <- screenGetRootWindow scr
    mPos <- drawWindowGetPointer root
    return $ check mPos
      where
          check (Just (True, x, y, _)) = (x,y)
          check _                      = (0,0)

