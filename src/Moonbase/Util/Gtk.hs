module Moonbase.Util.Gtk 
 ( iosync
 ) where

import Control.Monad.Except
import Graphics.UI.Gtk.General.General
 
iosync :: (MonadIO m) => IO a -> m a
iosync = liftIO . postGUISync

