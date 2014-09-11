{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Moonbase.Panel.Gtk
    ( PanelPosition(..)
    , Color
    , PanelItem(..)
    , Item(..)
    , PanelConfig(..)
    , GtkPanel(..)
    , gtkPanel
    ) where

import Control.Applicative
--import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as M

import Graphics.UI.Gtk hiding (get)

import Moonbase.Core
import Moonbase.Log
import Moonbase.Hook.Gtk

type GtkPanelT a = ComponentM GtkPanel a

data PanelPosition = Top
                   | Bottom
                   | Custom Int

class PanelItem a where
    initItem :: a -> GtkPanelT (a, Widget)
    getWidget :: a -> Widget

data Item = forall a. (PanelItem a) => Item Name Packing a

        
    
data PanelState = PanelState 
  { stItems :: M.Map String Item
  , stPanel :: Maybe Window
  , stHBox :: Maybe HBox
  }

emptyState :: PanelState
emptyState = PanelState
  { stItems = M.empty
  , stPanel = Nothing
  , stHBox  = Nothing
  }


data PanelConfig = PanelConfig 
  { screen :: Int
  , height :: Int
  , position :: PanelPosition
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



setPanelSize :: Rectangle -> Int -> Window -> GtkPanelT ()
setPanelSize
    (Rectangle _ _ w _) h win = io $ do
        windowSetDefaultSize win w h
        windowSetGeometryHints win no size size Nothing Nothing Nothing
        where
            no   = Nothing :: Maybe Widget
            size = Just (w, h)

setPanelPosition :: Rectangle -> Int -> PanelPosition -> Window -> GtkPanelT ()
setPanelPosition
    _ _ Top win = io $ windowMove win 0 0 
setPanelPosition
    (Rectangle _  _  _ h) hi Bottom win = io $ windowMove win 0 (h - hi)

panelAddItems :: [Item] -> Int -> HBox -> GtkPanelT (M.Map Name Item)
panelAddItems
    it _ box = M.fromList <$>  mapM append it
    where
        append (Item name p i) =  do
            (st, wid) <- initItem i
            io $ boxPackStart box wid p 0
            return (name, Item name p st)
        
startGtkPanel :: GtkPanelT Bool
startGtkPanel = do
     (GtkPanel conf st) <- get
     disp <- io displayGetDefault
     pa <- setupPanel disp conf st
     case pa of
        Nothing -> errorM "Could not get display. Creating gtkpanel failed" >> return False
        Just p  -> put p >> return True
    where
        setupPanel Nothing _ _ = return Nothing
        setupPanel (Just dsp) conf st = do
            scr <- io $ displayGetScreen dsp $ screen conf
            geo <- io $ screenGetMonitorGeometry scr 0 
            (win, box) <- createPanel conf

            setPanelSize geo (height conf) win
            setPanelPosition geo (height conf) (position conf) win

            i <- panelAddItems (items conf) (height conf) box
           

            io $ postGUIAsync $ widgetShowAll win
            return $ Just $ GtkPanel conf st
              { stPanel = Just win
              , stHBox = Just box
              , stItems = i }

        createPanel conf = io $ do
            win <- windowNew
            widgetSetName win "Panel"
            windowSetTypeHint win WindowTypeHintDock
            widgetModifyBg win StateNormal (background conf)
            widgetModifyFg win StateNormal (foreground conf)

            box <- hBoxNew False 2
            containerAdd win box

            return (win, box)
    

stopGtkPanel :: GtkPanelT ()
stopGtkPanel = destroy =<< (stPanel . gtkPanelState <$> get)
    where
        destroy Nothing = return ()
        destroy (Just pn) = io $ widgetDestroy pn

isGtkPanelRunning :: GtkPanelT Bool
isGtkPanelRunning = return True




