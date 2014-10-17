{-|
Module      : Moonbase.Theme
Copyright   : (c) Felix Schnizlein, 2014
License     : GPL-2
Maintainer  : felix@none.io
Stability   : experimental
Portability : POSIX

Some helper function to complete Gtk's functionality

-}

module Moonbase.Util.Gtk 
 ( iosync
 , withDisplay
 , pangoColor
 , pangoSanitize
 , Position(..)
 , moveWindow
 , setWindowHints
 , setWindowStruts
 , getAbsoluteMousePosition
 , parseColor
 , parseColor'
 ) where

import Control.Monad.Reader
import Control.Applicative

import Numeric (readHex)

import Data.Char (isHexDigit)

import qualified Graphics.UI.Gtk as Gtk

import Moonbase.Theme
import Moonbase
import Moonbase.Util.Gtk.StrutProperties


-- | Window positions
data Position = Top -- ^ At top
  | Bottom -- ^ At bottom
  | Custom Int -- ^ At a custom position


withDisplay :: (Functor m, MonadIO m, MonadSignal m) => (Gtk.Display -> m a) -> m (Maybe a)
withDisplay f = do
    disp <- liftIO $ Gtk.displayGetDefault
    case disp of
         Just d  -> Just <$> f d
         Nothing -> push (FatalError "Could not open display") >> return Nothing




-- | Wrapper arroung liftIO . Gtk.postGUISync
iosync :: (MonadIO m) => IO a -> m a
iosync = liftIO . Gtk.postGUISync


-- | Applys pango color formatting to a 'String'
pangoColor :: String -> String -> String
pangoColor fg str = left ++ str ++ right
    where
        left = "<span foreground=\"" ++ color_ fg ++ "\">"
        right = "</span>" 

-- | Sanatize few basic characters
pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>' xs = "&gt;" ++ xs
    sanitize '<' xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&' xs = "&amp;" ++ xs
    sanitize x xs = x:xs

-- | Move Window to given position
moveWindow :: Gtk.Window     -- ^ Window which should be moved
           -> Position       -- ^ Position where the window should moved
           -> Gtk.Rectangle  -- ^ Size of the monitor
           -> IO ()
moveWindow win pos (Gtk.Rectangle x _ _ h) = do
    (_, height) <- Gtk.windowGetSize win 
    Gtk.windowMove win x (offset height)
    where
        offset height = case pos of
                      Top            -> 0
                      Bottom         -> h - height
                      Custom height' -> h - height - height'

-- | Set window geometry hints (a easy wrapper for full horizontal windows)
setWindowHints :: Gtk.Window    -- ^ Window where geometry hints should set
               -> Gtk.Rectangle -- ^ Size of the monitor where the window is on
               -> IO ()
setWindowHints win (Gtk.Rectangle _ _ w _) = do
    (_, h) <- Gtk.windowGetSize win
    Gtk.windowSetGeometryHints win noWidget (Just (w,h)) (Just (w,h)) Nothing Nothing Nothing
      where
          noWidget = Nothing :: Maybe Gtk.Widget

-- | Generate strutProperties for fully horizontal windows
strutProperties :: Position        -- ^ Window position
                -> Int             -- ^ Window height
                -> Gtk.Rectangle   -- ^ Current monitor rectangle
                -> [Gtk.Rectangle] -- ^ All monitors
                -> StrutProperties
strutProperties pos bh (Gtk.Rectangle mX mY mW mH) monitors = propertize pos sX sW sH
    where
        sX = mX
        sW = mW - 1
        sH = case pos of
            Top -> bh + mY
            Bottom -> bh + totalH - mY - mH
        totalH = maximum $ map bottomY monitors
        bottomY (Gtk.Rectangle _ y _ h) = y + h
        propertize p x w h = case p of
            Top    -> StrutProperties 0 0 h 0 0 0 0 0 x (x+w) 0 0
            Bottom -> StrutProperties 0 0 0 h 0 0 0 0 0 0 x (x+w)

-- | Sets window struts
setWindowStruts :: Gtk.Window -> Position -> Int -> Gtk.Rectangle -> IO ()
setWindowStruts win pos height geo = do
    scr    <- Gtk.windowGetScreen win
    moNum  <- Gtk.screenGetNMonitors scr
    moGeos <- mapM (Gtk.screenGetMonitorGeometry scr) [0 .. (moNum - 1)]
    
    setStrutProperties win $ strutProperties pos height geo moGeos

-- | Returns the absolute mouse position
-- 
-- If the mouse pointer is not on the screen (which is usual the case with Xinerama and nvidia twinview)
-- this function return (0,0)
getAbsoluteMousePosition :: Gtk.Screen -> IO (Int, Int)
getAbsoluteMousePosition scr = do
    root <- Gtk.screenGetRootWindow scr
    mPos <- Gtk.drawWindowGetPointer root
    return $ check mPos
      where
          check (Just (True, x, y, _)) = (x,y)
          check _                      = (0,0)


-- | Parse a Hexdecimal color string into a Gtk.Color
parseColor :: Color -> Gtk.Color
parseColor ['#',r,g,b] 
    = parseColor ['#',r,r,g,g,b,b]
parseColor s@['#',r1,r2,g1,g2,b1,b2]
  | all isHexDigit (tail s) = Gtk.Color (gtkCol [r1,r2]) (gtkCol [g1,g2]) (gtkCol [b1,b2])
  | otherwise = parseColor magenta
  where gtkCol x = 257 * fst (head $ readHex x)
parseColor _ = parseColor magenta

-- | Parse hexadecimal color string into doubles
parseColor' :: Color -> (Double, Double, Double)
parseColor' ['#', r, g, b] = parseColor' ['#', r, r, b, b, g, g]
parseColor' s@['#', r1, r2, g1, g2, b1, b2]
  | all isHexDigit (tail s) = (hexify [r1,r2], hexify [g1, g2], hexify [b1, b2])
  | otherwise               = parseColor' magenta
  where hexify = fst . head . readHex
parseColor' _              = parseColor' magenta

