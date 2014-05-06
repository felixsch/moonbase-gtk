{-# LANGUAGE ExistentialQuantification #-}

module Moonbase.Panel.Gtk
    ( PanelPosition(..)
    , Color
    , PanelItem(..)
    , ItemWidth(..)
    , Item(..)
    , PanelConfig(..)
    , GtkPanel(..)
    , gtkPanel
    , sampleItem
    ) where

import Control.Monad
import Control.Applicative
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

data ItemWidth = Max
               | Size Int

class PanelItem a where
    initItem :: a -> Moonbase (a, Widget)
    getWidget :: a -> Widget

data Item = forall a. (PanelItem a) => Item Name ItemWidth a

data SampleItem = SampleItem String (Maybe Label)

instance PanelItem SampleItem where
    initItem (SampleItem n _)  = do
        l <- io $ labelNew (Just n)
        return (SampleItem n (Just l), toWidget l)
    getWidget (SampleItem _ (Just l)) = toWidget l

sampleItem :: String -> ItemWidth -> Item
sampleItem n w = Item n w (SampleItem n Nothing)
        
        
        
    
data PanelState = PanelState 
  { itms :: M.Map String Item
  , panel :: Maybe Window
  , hbox :: Maybe HBox
  }

emptyState :: PanelState
emptyState = PanelState
  { itms = M.empty
  , panel = Nothing
  , hbox  = Nothing
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

setPanelSize :: Rectangle -> Int -> Window -> Moonbase ()
setPanelSize
    (Rectangle _ _ w _) h win = io $ do
        windowSetDefaultSize win w h
        windowSetGeometryHints win no size size Nothing Nothing Nothing
        where
            no   = Nothing :: Maybe Widget
            size = Just (w, h)

setPanelPosition :: Rectangle -> Int -> PanelPosition -> Window -> Moonbase ()
setPanelPosition
    _ _ Top win = io $ windowMove win 0 0 
setPanelPosition
    (Rectangle _  _  _ h) hi Bottom win = io $ windowMove win 0 (h - hi)

panelAddItems :: [Item] -> Int -> HBox -> Moonbase (M.Map Name Item)
panelAddItems
    it h box = M.fromList <$>  mapM append it
    where
        append (Item name Max i) =  do
            (st, wid) <- initItem i
            io $ boxPackStart box wid PackGrow 0 >> widgetShow wid
            return (name, Item name Max st)
        append (Item name (Size w) i) = do
            (st, wid) <- initItem i
            io $ void $ widgetSizeAllocate wid (Rectangle 0 0 w h)
            io $ boxPackStart box wid PackNatural 0 >> widgetShow wid

            return (name, Item name (Size w) st)
            


                
                
            

startGtkPanel :: GtkPanel -> Moonbase GtkPanel
startGtkPanel
    p@(GtkPanel conf st)  = setupPanel =<< io displayGetDefault
    where
        setupPanel Nothing = errorM "Could not get display. Creating gtk panel failed." >> return p
        setupPanel (Just dsp) = do
            scr <- io $ displayGetScreen dsp $ screen conf
            geo <- io $ screenGetMonitorGeometry scr 0 
            (win, box) <- createPanel

            setPanelSize geo (height conf) win
            setPanelPosition geo (height conf) (position conf) win

            i <- panelAddItems (items conf) (height conf) box
           

            io $ postGUIAsync $ widgetShow box >> widgetShow win
            return $ GtkPanel conf st { panel = Just win, hbox = Just box, itms = i}

        createPanel = io $ do
            win <- windowNew
            widgetSetName win "Panel"
            windowSetTypeHint win WindowTypeHintDock
            widgetModifyBg win StateNormal (background conf)
            widgetModifyFg win StateNormal (foreground conf)

            box <- hBoxNew False 2
            containerAdd win box

            return (win, box)
    

stopGtkPanel :: GtkPanel -> Moonbase ()
stopGtkPanel
    (GtkPanel _ st) = destroy $ panel st
        where
            destroy Nothing = return ()
            destroy (Just pn) = io $ widgetDestroy pn

isGtkPanelRunning :: GtkPanel -> Moonbase Bool
isGtkPanelRunning
    _ = return True





