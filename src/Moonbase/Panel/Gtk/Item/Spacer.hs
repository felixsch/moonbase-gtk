module Moonbase.Panel.Gtk.Item.Spacer
    ( spacer
    , (<-->)
    ) where

import Graphics.UI.Gtk

import Moonbase.Core
import Moonbase.Panel.Gtk
import Moonbase.Util.Gtk

data ItemSpacer = ItemSpacer (Maybe Label)

instance PanelItem ItemSpacer where
    initItem _ = do
     sp <- io $ labelNew' Nothing
     iosync $ widgetShow sp
     return (ItemSpacer (Just sp), toWidget sp)
    getWidget (ItemSpacer (Just sp)) = toWidget sp


labelNew' :: Maybe String -> IO Label
labelNew' = labelNew

spacer :: Item
spacer = Item "spacer" PackGrow (ItemSpacer Nothing)

(<-->) :: Item
(<-->) = spacer


