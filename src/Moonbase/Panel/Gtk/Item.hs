module Moonbase.Panel.Gtk.Item
    ( date
    , spacer
    , quitButton
    , xmonadLog
    , dbusLabel
    , cpuGraph
    , defaultGraphConfig
    ) where

import Moonbase.Util.Gtk.Widget.Graph (defaultGraphConfig)


import Moonbase.Panel.Gtk.Item.Spacer
import Moonbase.Panel.Gtk.Item.Date
import Moonbase.Panel.Gtk.Item.QuitButton
import Moonbase.Panel.Gtk.Item.DbusLabel
import Moonbase.Panel.Gtk.Item.CpuGraph
