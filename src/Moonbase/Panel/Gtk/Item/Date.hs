module Moonbase.Panel.Gtk.Item.Date
    ( date
    , ItemDate(..)
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad (forever, void)

import System.Locale (defaultTimeLocale)

import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)

import Graphics.UI.Gtk

import Moonbase.Core
import Moonbase.Panel.Gtk
import Moonbase.Util.Gtk



data ItemDate = ItemDate String (Maybe Label)

instance PanelItem ItemDate where
    getWidget (ItemDate _ (Just l)) = toWidget l
    initItem (ItemDate fmt _) = do
        l <- io $ labelNew (Just "-")

        _ <- io $ on l realize $ void $
            forkIO $ forever $ do
                labelSetLabel l =<< formatTime defaultTimeLocale fmt <$> getCurrentTime
                threadDelay 1000000 -- one second 

        iosync $ widgetShow l

        return (ItemDate fmt (Just l), toWidget l) 

date :: String -> Item
date fmt = Item ("date=" ++ fmt) PackNatural $ ItemDate fmt Nothing
        
        

