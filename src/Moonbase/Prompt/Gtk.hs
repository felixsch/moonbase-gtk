{-# LANGUAGE OverloadedStrings #-}

module Moonbase.Prompt.Gtk
  ( prompt
  , PromptTheme(..)
  ) where


import Moonbase
import Moonbase.Item
import Moonbase.Theme
import Moonbase.Hook.Gtk
import Moonbase.Util.Gtk.Widget.Prompt
import Moonbase.Util.Gtk

import DBus
import DBus.Client


prompt :: (PromptTheme -> PromptTheme) -> Moonbase ()
prompt gen = do
    theme <- getTheme

    addHooks [gtkInit, gtkMain, gtkQuit]

    withComponent Low "testPrompt" $ newComponent Nothing (initPrompt $ genTheme theme)

 where
     genTheme theme = gen (basicTheme theme)
     basicTheme theme = PromptTheme
       { promptBackground = bg theme
       , promptForeground = normalC theme
       , promptHeight     = 20
       , promptPosition   = Bottom }



initPrompt :: PromptTheme -> ComponentM (Maybe Prompt) ()
initPrompt theme = do
    prompt  <- io $ promptNew theme

    moon $ addDBusMethod (withObjectPath "Prompt") $ \ ref ->
      [ autoMethod promptInterface "Show" (wrap0 ref (ioasync $ promptShow prompt))
      , autoMethod promptInterface "Hide" (wrap0 ref (ioasync $ promptHide prompt)) ]

    return ()

promptInterface = interfaceName_ $ formatInterfaceName moonInterface ++ ".TestPrompt"
    




