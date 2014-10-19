module Moonbase.Hook.Gtk
    ( gtkInit
    , gtkMain
    , gtkQuit
    ) where

import Control.Monad
import Control.Concurrent

import Graphics.UI.Gtk
import Moonbase


gtkInit :: Hook
gtkInit = Hook "gtkinit" HookInit $ io (void initGUI)

gtkMain :: Hook 
gtkMain = Hook "gtkmain" HookStart $ io $ void $ forkOS mainGUI

gtkQuit :: Hook
gtkQuit = Hook "gtkquit" HookExit $ io mainQuit


