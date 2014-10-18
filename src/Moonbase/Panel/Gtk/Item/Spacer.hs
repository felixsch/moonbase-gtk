module Moonbase.Panel.Gtk.Item.Spacer
    ( spacer
    ) where

import qualified Graphics.UI.Gtk as Gtk

import Moonbase
import Moonbase.Item
import Moonbase.Panel.Gtk

spacer :: GtkPanelItem
spacer = item $ do
    label <- io $ labelNew' Nothing
    return (Gtk.toWidget label, Gtk.PackGrow)
  where
      labelNew' :: Maybe String -> IO Gtk.Label
      labelNew' = Gtk.labelNew
