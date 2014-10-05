module Moonbase.Util.Gtk.Widget.Prompt
        (
        ) where



type PromptComplList = Liststore (String, String -> IO ())


type Prompt = Widget

promptNew :: IO Prompt


promptShow :: Prompt -> IO ()
promptHide :: Prompt -> IO ()

promptSetList :: Prompt -> PromptComplList -> IO ()
promptGetList :: Prompt -> IO PromptComplList



