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


data ItemQuitButton = ItemQuitButton (Maybe Button)

instance PanelItem ItemQuitButton where
    getWidget (ItemQuitButton (Just b)) = toWidget b
    initItem _ = do
        b <- io $ buttonNewFromStock stockQuit
        _ <- io $ on b buttonActivated $ do
            client <- connectSession
            callNoReply client (methodCall "/" "org.Moonbase.Core" "Quit")
                { methodCallDestination = Just "org.Moonbase.Core"
                }


        return (ItemQuitButton (Just b), toWidget b)

quitButton :: Item
quitButton = Item "QuitButton" PackNatural (ItemQuitButton Nothing)
        
