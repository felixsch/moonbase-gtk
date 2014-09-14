{-# LANGUAGE OverloadedStrings #-}

module Moonbase.Panel.Gtk.Item.DbusLabel
  ( ItemDBusLabel(..)
  , xmonadLog
  , dbusLabel
  ) where

import Control.Monad

import Moonbase.Core
import Moonbase.Panel.Gtk

import DBus
import DBus.Client

import Graphics.UI.Gtk

data ItemDBusLabel = ItemDBusLabel MatchRule (Maybe Label)

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





        
