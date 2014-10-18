{-# LANGUAGE ExistentialQuantification #-}

module Moonbase.Desktop.Gtk
  ( gtkDesktop
  , onEvery
  , wallpaper
  ) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State 

import System.Directory
import System.Glib.GError

import Data.Maybe

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.Rendering.Cairo as Cairo

import Moonbase
import Moonbase.Item
import Moonbase.Theme
import Moonbase.Util.Gtk


import Moonbase.Hook.Gtk


type GtkDesktop = Maybe [Gtk.Window]

type GtkDesktopItem = Item (ComponentM GtkDesktop) ()



data BackgroundImage = BackgroundImage
  { biMonitor :: Int
  , biScreen  :: Maybe Int
  , biImage   :: FilePath }


gtkDesktop :: GtkDesktopItem -> Moonbase ()
gtkDesktop (Item desktop) = withComponent "gtkDesktop" $ newComponent Nothing $ do

    withHooks [gtkInit, gtkMain, gtkQuit]

    theme   <- moon $ getTheme

    windows <- withDisplay $ \disp -> do
        setupCursor disp Gtk.Arrow
        screens <- liftIO $ getScreens disp
        monitorGeos <- liftIO $ getMonitorGeos screens

        liftIO $ forM (concat monitorGeos) $ \(screen, geo) -> do
          win <- Gtk.windowNew

          Gtk.windowSetScreen win screen

          Gtk.widgetModifyBg  win Gtk.StateNormal (parseColor $ bg theme)
          Gtk.widgetModifyFg  win Gtk.StateNormal (parseColor $ normalC theme)

          setupDesktop        win geo

          return win

    case windows of
         Nothing -> push (InitFailed True "Could not open display")
         Just x -> do
             put (Just x)
             sequence_ desktop
             mapM_ (iosync . Gtk.widgetShowAll) x
  where
      getScreens disp = do
          screenNum <- Gtk.displayGetNScreens disp
          mapM (Gtk.displayGetScreen disp) [0..screenNum -1]

      getMonitorGeos = mapM $ \screen -> do
          monitorNum <- Gtk.screenGetNMonitors screen
          forM [0..monitorNum - 1] $ \i -> do
              geo <- Gtk.screenGetMonitorGeometry screen i
              return (screen, geo)

                
setupCursor :: Gtk.Display -> Gtk.CursorType -> ComponentM st ()
setupCursor disp cursorType = liftIO $ do
    cursor    <- Gtk.cursorNewForDisplay disp cursorType
    screenNum <- Gtk.displayGetNScreens disp
    forM_ [0 .. (screenNum - 1)] $ \i -> do
        scr  <- Gtk.displayGetScreen disp i
        root <- Gtk.screenGetRootWindow scr
        Gtk.drawWindowSetCursor root (Just cursor)



setupDesktop :: Gtk.Window -> Gtk.Rectangle -> IO ()
setupDesktop win (Gtk.Rectangle x y w h) = do
    Gtk.widgetSetName win "desktop"
    
    Gtk.windowSetTypeHint win Gtk.WindowTypeHintDesktop
    Gtk.windowSetGravity  win Gtk.GravityStatic
    Gtk.widgetSetCanFocus win False

    Gtk.windowSetDefaultSize win w h
    Gtk.widgetSetSizeRequest win w h
    Gtk.windowResize win w h

    Gtk.windowMove win 0 0
    Gtk.windowSetGeometryHints win noWidget (Just (w,h)) (Just (w,h)) Nothing Nothing Nothing
    where
      noWidget                 = Nothing :: Maybe Gtk.Widget


getDesktop :: Int -> ComponentM GtkDesktop (Maybe Gtk.Window)
getDesktop i = do
    windows <- get
    case windows of
         Nothing   -> return Nothing
         Just wins -> return $ if (null wins || i < 0 || i > length wins)
                                  then Nothing
                                  else Just $ wins !! i

onEvery :: (Int -> GtkDesktopItem) -> GtkDesktopItem
onEvery f = item $ do
    mWindows <- get
    case mWindows of
         Nothing   -> return ()
         Just wins -> forM_ [0.. length wins] $ \i -> do
             let (Item item') = f i
             sequence_ item'





wallpaper :: FilePath -> Int -> GtkDesktopItem
wallpaper imagePath i = item $ do
    mWindow <- getDesktop i
    case mWindow of
         Nothing  -> push (Warning $ "Invalid monitor specified: " ++ show i)
         Just win -> do
             imageExists <- liftIO $ doesFileExist imagePath
             if (not imageExists) 
                then push (Warning $ "Could not find wallpaper image: " ++ imagePath)
                else do
                    size   <- liftIO $ Gtk.windowGetSize win
                    image  <- loadImage imagePath size
                    widget <- liftIO $ Gtk.imageNewFromPixbuf image
                    liftIO $ Gtk.containerAdd win widget




loadImage :: FilePath -> (Int, Int) -> ComponentM GtkDesktop Gtk.Pixbuf
loadImage path (w,h) = io $ catchGErrorJustDomain (load path) (blackPixbuf w h)
    where
        load img  = Gtk.pixbufNewFromFileAtScale img w h False
         
blackPixbuf :: Int -> Int -> Gtk.PixbufError -> GErrorMessage -> IO Gtk.Pixbuf
blackPixbuf w h _ _= do
    pixbuf <- Gtk.pixbufNew Gtk.ColorspaceRgb False 8 w h
    Gtk.pixbufFill pixbuf 0 0 0 255
    
    return pixbuf

{-
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

setupCursor :: (Component st) => Cursor -> ComponentM st ()
setupCursor cursor = do
    disp      <- checkDisplay =<< io displayGetDefault
    screenNum <- io $ displayGetNScreens disp
    forM_ [0 .. (screenNum - 1)] $ \i -> io $ do
        scr <- displayGetScreen disp i
        root <- screenGetRootWindow scr
        drawWindowSetCursor root (Just cursor)

    where 
      checkDisplay (Just disp) = return disp
      checkDisplay _           = throwError (InitFailed "BackgroundImage: Could not open display")

-}
