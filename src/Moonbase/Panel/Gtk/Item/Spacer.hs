module Moonbase.Panel.Gtk.Item.Spacer
    ( spacer
    , (<-->)
    ) where

import Graphics.UI.Gtk

import Moonbase.Core
import Moonbase.Panel.Gtk

data ItemSpacer = ItemSpacer (Maybe Label)

instance PanelItem ItemSpacer where
    initItem _ = do
     sp <- io $ labelNew Nothing
     return (ItemSpacer (Just sp), toWidget sp)
    getWidget (ItemSpacer (Just sp)) = toWidget sp

spacer :: Item
spacer = Item "spacer" PackGrow (ItemSpacer Nothing)

(<-->) :: Item
(<-->) = spacer


