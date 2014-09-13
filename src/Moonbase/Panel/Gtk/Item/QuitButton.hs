{-# LANGUAGE OverloadedStrings #-}

module Moonbase.Panel.Gtk.Item.QuitButton
    ( quitButton
    , ItemQuitButton(..)
    ) where


import Graphics.UI.Gtk
import DBus
import DBus.Client

import Moonbase.Core (io)
import Moonbase.Panel.Gtk
import Moonbase.Util.Gtk


data ItemQuitButton = ItemQuitButton (Maybe Button)

instance PanelItem ItemQuitButton where
    getWidget (ItemQuitButton (Just b)) = toWidget b
    initItem _ = do
        b <- iosync $ buttonNewWithLabel ("Quit" :: String)
        
        _ <- io $ on b buttonActivated $ do
            client <- connectSession
            callNoReply client (methodCall "/" "org.Moonbase.Core" "Quit")
                { methodCallDestination = Just "org.Moonbase.Core"
                }

        iosync $ widgetShow b


        return (ItemQuitButton (Just b), toWidget b)

quitButton :: Item
quitButton = Item "QuitButton" PackNatural (ItemQuitButton Nothing)
        
