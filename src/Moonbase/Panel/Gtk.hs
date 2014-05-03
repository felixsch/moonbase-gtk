{-# LANGUAGE ExistentialQuantification #-}

module Moonbase.Panel.Gtk
    ( PanelPosition(..)
    , Color
    , PanelItem(..)
    , Item(..)
    , PanelConfig(..)
    , GtkPanel(..)
    , gtkPanel
    ) where

--import Control.Applicative
--import Control.Monad.Reader
--import Control.Monad.State

import qualified Data.Map as M

import Graphics.UI.Gtk

import Moonbase.Core
import Moonbase.Log
import Moonbase.Hook.Gtk

data PanelPosition = Top
                   | Bottom
                   | Custom Int

class PanelItem a where
    stateInit :: Moonbase a
    init :: a -> Moonbase a
    show :: a -> Moonbase ()

data Item = forall a. (PanelItem a) => Item String a

data ItemState = forall a. ItemState a



data PanelState = PanelState 
  { itemStates :: M.Map String ItemState
  , panel :: Maybe Window
  }

emptyState :: PanelState
emptyState = PanelState
  { itemStates = M.empty
  , panel = Nothing
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
gtkPanel conf = Panel "GtkPanel" (GtkPanel conf emptyState)


data GtkPanel = GtkPanel PanelConfig PanelState

instance StartStop GtkPanel where
    start = startGtkPanel
    stop  = stopGtkPanel
    isRunning = isGtkPanelRunning

instance Requires GtkPanel where
    requires _ = [gtkInit, gtkMain, gtkQuit]



startGtkPanel :: GtkPanel -> Moonbase GtkPanel
startGtkPanel
    p@(GtkPanel conf st)  = setupPanel =<< io displayGetDefault
    where
        setupPanel Nothing = errorM "Could not get display. Creating gtk panel failed." >> return p
        setupPanel (Just dsp) = do
            scr <- io $ displayGetScreen dsp $ screen conf
            geomentry <- io $ screenGetMonitorGeometry scr 0
            
            pw <- createPanel geomentry

            io $ postGUIAsync $ widgetShow pw
            return $ GtkPanel conf st { panel = Just pw }
        createPanel (Rectangle x y w h) = io $ do
            window <- windowNew
            widgetSetName window "Panel"
            windowSetTypeHint window WindowTypeHintDock
            windowSetDefaultSize window w (height conf)
            widgetModifyBg window StateNormal (background conf)
            widgetModifyFg window StateNormal (foreground conf)
            
            windowMove window x (y + case position conf of
                Top -> 0
                Bottom -> h - height conf)
            return window


stopGtkPanel :: GtkPanel -> Moonbase ()
stopGtkPanel
    (GtkPanel _ st) = destroy $ panel st
        where
            destroy Nothing = return ()
            destroy (Just pn) = io $ widgetDestroy pn

isGtkPanelRunning :: GtkPanel -> Moonbase Bool
isGtkPanelRunning
    _ = return True





