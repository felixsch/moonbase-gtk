{-# LANGUAGE ExistentialQuantification #-}

module Moonbase.Panel.Gtk
    ( PanelPosition(..)
    , Color
    , PanelItem(..)
    , Item(..)
    , PanelConfig(..)
    , GtkPanel(..)
    ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as M

import Graphics.UI.Gtk

import Moonbase.Core

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
  }

data PanelConfig = PanelConfig 
  { screen :: Int
  , height :: Int
  , position :: PanelPosition
  , background :: Color
  , items :: [Item]
  }



data GtkPanel = GtkPanel PanelConfig

instance StartStop GtkPanel where
    start = startGtkPanel
    stop  = stopGtkPanel
    isRunning = isGtkPanelRunning


startGtkPanel :: GtkPanel -> Moonbase GtkPanel
startGtkPanel
    = do
        
        

stopGtkPanel :: GtkPanel -> Moonbase ()
stopGtkPanel
    = undefined

isGtkPanelRunning :: GtkPanel -> Moonbase Bool
isGtkPanelRunning
    = undefined





