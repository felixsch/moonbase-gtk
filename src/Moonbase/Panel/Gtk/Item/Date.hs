module Moonbase.Panel.Gtk.Item.Date
    ( date
    , dateWith
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad (forever, void)


import Data.Time.Format
import Data.Time.LocalTime

import qualified Graphics.UI.Gtk as Gtk

import Moonbase
import Moonbase.Theme
import Moonbase.Item
import Moonbase.Panel.Gtk
import Moonbase.Util.Gtk


date :: String -> GtkPanelItem
date fmt = item $ do
        label <- io $ createDateWidget fmt 1 Nothing  
        return (Gtk.toWidget label, Gtk.PackNatural)

dateWith :: String -> Int -> Color -> GtkPanelItem
dateWith fmt poll color = item $ do
        label <- io $ createDateWidget fmt poll (Just color)
        return (Gtk.toWidget label, Gtk.PackNatural)


createDateWidget :: String -> Int -> Maybe Color -> IO Gtk.Label
createDateWidget fmt poll color = do
        l <- io $ Gtk.labelNew (Just "-")
        io $ Gtk.labelSetUseMarkup l True
        _ <- io $ Gtk.on l Gtk.realize $ void $
            forkIO $ forever $ do
                str <- formatTime defaultTimeLocale fmt <$> getZonedTime 
                Gtk.postGUISync $ Gtk.labelSetMarkup l $ format str
                threadDelay (1000000 * poll)
        return l
  where
      format str = case color of
                        Just x  -> pangoColor x str
                        Nothing -> str




