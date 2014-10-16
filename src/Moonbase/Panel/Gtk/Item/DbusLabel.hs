{-# LANGUAGE OverloadedStrings #-}

module Moonbase.Panel.Gtk.Item.DbusLabel
  ( xmonadLog
  , dbusLabel
  ) where

import Control.Monad
import Control.Monad.State

import Moonbase
import Moonbase.Item
import Moonbase.Panel.Gtk

import DBus
import DBus.Client

import qualified Graphics.UI.Gtk as Gtk


xmonadLog :: GtkPanelItem
xmonadLog = dbusLabel rule
    where
        rule      = matchAny 
          { matchPath      = Just path
          , matchInterface = Just interface
          , matchMember    = Just member }
        path      = DBus.objectPath_ "/org/moonbase/XMonadLog"
        interface = DBus.interfaceName_ "org.xmonad.XMonadLog"
        member    = DBus.memberName_ "Update"


dbusLabel :: MatchRule -> GtkPanelItem
dbusLabel match = item $ do
        rt    <- moon $ get
        label <- io $ Gtk.labelNew (Just "waiting for xmonad...")

        void $ io $ addMatch (dbus rt) match $ \signal -> do
            let Just str = fromVariant $ head (signalBody signal)
            Gtk.postGUISync $ Gtk.labelSetMarkup label str

        return (Gtk.toWidget label, Gtk.PackNatural)


{-
instance PanelItem ItemDBusLabel where
    getWidget (ItemDBusLabel _  (Just l) ) = toWidget l
    initItem (ItemDBusLabel match _) = do
        l <- io $ labelNew (Just "waiting for xmonad...")

        dbus <- io connectSession

        void $ io $ addMatch dbus match $ \signal -> do
            let Just str = fromVariant $ head (signalBody signal)
            postGUISync $ labelSetMarkup l str

        io $ widgetShow l

        return (ItemDBusLabel match (Just l), toWidget l) 

xmonadLog :: Item
xmonadLog = Item "xmonadLog" PackNatural $ ItemDBusLabel rule Nothing
    where
        rule      = matchAny 
          { matchPath      = Just path
          , matchInterface = Just interface
          , matchMember    = Just member }
        path      = DBus.objectPath_ "/org/xmonad/Log"
        interface = DBus.interfaceName_ "org.xmonad.Log"
        member    = DBus.memberName_ "Update"

dbusLabel :: MatchRule -> Item
dbusLabel rule = Item "dbusLabel" PackNatural $ ItemDBusLabel rule Nothing
-}




        
