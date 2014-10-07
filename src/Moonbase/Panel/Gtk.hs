{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Moonbase.Panel.Gtk
    ( Color
    , PanelItem(..)
    , Item(..)
    , PanelConfig(..)
    , GtkPanel(..)
    , gtkPanel
    ) where

import Control.Applicative
--import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as M

import Graphics.UI.Gtk hiding (get)

import Moonbase.Core
import Moonbase.Hook.Gtk
import Moonbase.Util.Gtk
import Moonbase.Log



type GtkPanelT a = ComponentM GtkPanel a

{-
data PanelPosition = Top
                   | Bottom
                   | Custom Int
-}

class PanelItem a where
    initItem :: a -> GtkPanelT (a, Widget)
    getWidget :: a -> Widget

data Item = forall a. (PanelItem a) => Item Name Packing a

        
    
data PanelState = PanelState 
  { stItems :: [Item]
  , stPanel :: Maybe Window
  , stHBox :: Maybe HBox
  }

emptyState :: PanelState
emptyState = PanelState
  { stItems = []
  , stPanel = Nothing
  , stHBox  = Nothing
  }


data PanelConfig = PanelConfig 
  { monitor :: Int
  , height :: Int
  , spanPanel :: Bool
  , aboveAll :: Bool
  , position :: Position
  , background :: Color
  , foreground :: Color
  , items :: [Item]
  }


gtkPanel :: PanelConfig -> Panel
gtkPanel conf = Panel "GtkPanel" [gtkInit, gtkMain, gtkQuit] (GtkPanel conf emptyState)


data GtkPanel = GtkPanel
  { gtkPanelConfig :: PanelConfig
  , gtkPanelState :: PanelState }

instance Component GtkPanel where
    start = startGtkPanel
    stop  = stopGtkPanel
    isRunning = isGtkPanelRunning


panelAddItems :: [Item] -> HBox -> GtkPanelT [Item]
panelAddItems
    it box = mapM append it
    where
        append (Item name p i) =  do
            (st, wid) <- initItem i
            io $ boxPackStart box wid p 0
            return $ Item name p st

{-
strutProperties :: PanelPosition -- ^ Bar position
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
-}
{-
calcStrut :: PanelPosition -> Int -> Rectangle -> [Rectangle] -> StrutProperties
calcStrut pos ph (Rectangle sX sY sW sH) screens = genProperties pos sX (sW - 1) cH
    where
        cH     = case pos of 
            Top      -> ph + sY
            Bottom   -> ph + totalH - sY - sH
            Custom x -> ph + x + totalH -sY - sH
        totalH = maximum $ map bottom screens
        bottom (Rectangle _ y _ h) = y + h
        genProperties p x w h = case p of
            Top       -> newStrutProp { top = h, leftStartX = x, leftStartY = x + w }
            otherwise -> newStrutProp { bottom = h, rightStartX = x, rightEndX = x + w }
-}
            
            
startGtkPanel :: GtkPanelT Bool
startGtkPanel = do

    p     <- get
    disp  <- checkDisplay =<< io displayGetDefault

    (win, box)  <- io $ createPanel p disp

    items'      <- panelAddItems (items $ gtkPanelConfig p) box

    io $ widgetShowAll win
    put $ p { gtkPanelState = PanelState 
      { stItems = items'
      , stPanel = Just win
      , stHBox  = Just box }}
    moon $ infoM "foo 5 "

    return True
    where
        checkDisplay (Just disp) = return disp
        checkDisplay _           = moon $ throwError (InitFailed "Could not open display")
    

createPanel :: GtkPanel -> Display -> IO (Window, HBox)
createPanel (GtkPanel conf st) disp = do

     scr       <- displayGetScreen disp $ monitor conf
     screenNum <- displayGetNScreens disp

     win       <- windowNew

     widgetSetName win "panel"

     windowSetScreen   win scr
     windowSetTypeHint win WindowTypeHintDock 
     windowSetGravity  win GravityStatic

     
     widgetSetCanFocus win False
     widgetModifyBg    win StateNormal (background conf)
     widgetModifyFg    win StateNormal (foreground conf)

     setPanelSize conf win

     _ <- on scr screenMonitorsChanged $ setPanelSize conf win

     box <- hBoxNew False 2
     containerAdd win box

     return (win, box)

setPanelSize :: PanelConfig -> Window -> IO ()
setPanelSize conf win = do
   scr      <- windowGetScreen win

   moSelGeo@(Rectangle x y w h) <- screenGetMonitorGeometry scr (monitor conf)

   windowSetDefaultSize win w (height conf)
   widgetSetSizeRequest win w (height conf)
   windowResize win w (height conf)

   moveWindow win (position conf) (height conf) moSelGeo
   setWindowHints win moSelGeo (height conf)

   _ <- on win realize $ setWindowStruts win (position conf) (height conf) moSelGeo

   isRealized <- widgetGetRealized win
   when isRealized $ setWindowStruts win (position conf) (height conf) moSelGeo

   
{-
setPanelHints :: Window -> Rectangle -> Int -> IO ()
setPanelHints win (Rectangle _ _ w _) height =
    windowSetGeometryHints win noWidget size size Nothing Nothing Nothing
    where
        noWidget = Nothing :: Maybe Widget
        size = Just (w, height)

setPanelStrut :: Window -> PanelPosition -> Int -> Rectangle -> [Rectangle] -> IO ()
setPanelStrut win pos height geo geos = setStrutProperties win $
            strutProperties pos height geo geos
   
movePanel :: Window -> PanelPosition -> Int -> Rectangle -> IO ()
movePanel win pos height (Rectangle x _ _ h) = windowMove win x offset
    where
        offset = case pos of
            Top            -> 0
            Bottom         -> h - height
            Custom height' -> h - height - height' -}


stopGtkPanel :: GtkPanelT ()
stopGtkPanel = destroy =<< (stPanel . gtkPanelState <$> get)
    where
        destroy Nothing = return ()
        destroy (Just pn) = io $ widgetDestroy pn

isGtkPanelRunning :: GtkPanelT Bool
isGtkPanelRunning = return True




