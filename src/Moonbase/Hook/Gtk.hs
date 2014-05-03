module Moonbase.Hook.Gtk
    ( gtkInit
    , gtkMain
    , gtkQuit
    ) where

import Control.Monad
import Control.Concurrent

import Graphics.UI.Gtk
import Moonbase.Core


gtkInit :: Hook
gtkInit = Hook "gtkinit" HookStart $ io (void initGUI)

gtkMain :: Hook 
gtkMain = Hook "gtkmain" HookAfterStartup $ io $ void $ forkIO mainGUI

gtkQuit :: Hook
gtkQuit = Hook "gtkquit" HookQuit $ io mainQuit


