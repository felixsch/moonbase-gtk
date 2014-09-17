
module Moonbase.Desktop.Gtk.Background
  ( justImage
  , BackgroundImage (..)
  ) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State 

import System.Glib.GError

import Data.Maybe

import Graphics.UI.Gtk hiding ( get )
import Graphics.Rendering.Cairo

import Moonbase.Core
import Moonbase.Log


import Moonbase.Hook.Gtk



data BackgroundImage = BackgroundImage
  { biMonitor :: Int
  , biScreen  :: Maybe Int
  , biImage   :: FilePath }


instance Component BackgroundImage where
    start     = backgroundStart
    stop      = backgroundStop

    isRunning = return True

backgroundStart :: ComponentM BackgroundImage Bool
backgroundStart = do
    conf <- get

    disp <- checkDisplay =<< io displayGetDefault

    scr <- getScreen disp (biScreen conf)

    geo@(Rectangle x y w h)  <- io $ screenGetMonitorGeometry scr (biMonitor conf) -- Add monitor check

    win <- io windowNew

    io $ windowSetScreen   win scr
    io $ setupDesktop      win w h


    image <- loadImage (biImage conf) geo

    imageWidget <- io $ imageNewFromPixbuf image

    -- setup basic cursor
    
    cursor <- io $ cursorNewForDisplay disp Arrow

    setupCursor cursor


    io $ containerAdd win imageWidget
    io $ widgetQueueDraw win
    io $ widgetShowAll win

    return True

    where
      checkDisplay (Just disp) = return disp
      checkDisplay _           = throwError (InitFailed "BackgroundImage: Could not open display")


setupDesktop :: Window -> Int -> Int -> IO ()
setupDesktop win w h = do
    widgetSetName win "desktop"
    
    windowSetTypeHint win WindowTypeHintDesktop
    windowSetGravity  win GravityStatic
    widgetSetCanFocus win False

    windowSetDefaultSize win w h
    widgetSetSizeRequest win w h
    windowResize win w h

    windowMove win 0 0
    windowSetGeometryHints win noWidget (Just (w,h)) (Just (w,h)) Nothing Nothing Nothing
    where
      noWidget                 = Nothing :: Maybe Widget
     


getScreen :: Display -> Maybe Int -> ComponentM BackgroundImage Screen
getScreen disp Nothing  = io $ displayGetDefaultScreen disp
getScreen disp (Just i) = do 
    sNum <- io $ displayGetNScreens disp

    when (i > sNum || i < 0) $ throwError (InitFailed "BackgroundImage: Invalid screen number")

    io $ displayGetScreen disp i


loadImage :: FilePath -> Rectangle -> ComponentM BackgroundImage Pixbuf
loadImage path (Rectangle x y w h) = io $ catchGErrorJustDomain (load path) (blackPixbuf w h)
    where
        load img  = pixbufNewFromFileAtScale img w h False
        
        
blackPixbuf :: Int -> Int -> PixbufError -> GErrorMessage -> IO Pixbuf
blackPixbuf w h _ _= do
    pixbuf <- pixbufNew ColorspaceRgb False 8 w h
    pixbufFill pixbuf 0 0 0 255
    
    return pixbuf




drawImage :: DrawWindow -> Rectangle -> Pixbuf -> IO ()
drawImage area geo@(Rectangle x y w h) image = do
    drawWindowBeginPaintRect area geo 

    renderWithDrawWindow area $ setSourcePixbuf image (fromIntegral x) (fromIntegral y) >> paint >> fill
    
    drawWindowEndPaint area
    


backgroundStop :: ComponentM BackgroundImage ()
backgroundStop = return () -- implement me!

justImage :: FilePath -> Int -> Maybe Int -> Desktop
justImage path monitor mScreen = Desktop "background-image" [gtkInit, gtkMain, gtkQuit] $ BackgroundImage monitor mScreen path



