module Moonbase.Util.Gtk.Widget.Prompt
        (
        ) where

import Moonbase.Util.Gtk.Color

import Graphics.UI.Gtk

import System.IO.Unsafe

import Data.Maybe
import qualified Data.Map as M



type PromptComplFunc = (String -> IO [String])
type PromptExecFunc  = (String -> IO ())

data PromptExtension = PromptExtentsion String PromptComplFunc PromptExecFunc

data PromptTheme = PromptTheme {
    promptBackground :: HexColor
}


type Prompt = Widget

maybeWidgets :: Attr Prompt (Maybe (M.Map String Widget))
maybeWidgets = unsafePerformIO $ objectCreateAttribute
{-# NOINLINE maybeWidgets #-}


promptWidgets :: Attr Prompt (M.Map String Widget)
promptWidgets = newAttr getWidgets setWidgets
    where
        getWidgets obj = do
            mWidgets <- get obj maybeWidgets
            return $ fromMaybe M.empty mWidgets

        setWidgets obj widgets = do
            set obj [maybeWidgets := Just widgets]

maybeExtensions :: Attr Prompt (Maybe (M.Map String PromptExtension))
maybeExtensions = unsafePerformIO $ objectCreateAttribute
{-# NOINLINE maybeExtensions #-}

promptExtensions :: Attr Prompt (M.Map String PromptExtension)
promptExtensions = newAttr getExts setExts
    where
        getExts obj = do
            mExts <- get obj maybeExtensions
            return $ fromMaybe M.empty mExts
        setExts obj exts = set obj [maybeExtensions := Just exts]

maybeTheme :: Attr Prompt (Maybe PromptTheme)
maybeTheme = unsafePerformIO $ objectCreateAttribute
{-# NOINLINE maybeTheme #-}

promptTheme :: ReadAttr Prompt PromptTheme
promptTheme = readAttr $ \obj -> do
    mTheme <- get obj maybeTheme
    if isNothing mTheme
       then error $ "Could not load theme..."
       else return $ fromJust mTheme



promptNew :: PromptTheme -> IO Prompt
promptNew = undefined

promptShow :: Prompt -> IO ()
promptShow = undefined

promptHide :: Prompt -> IO ()
promptHide = undefined

promptAddEx :: Prompt -> PromptExtension -> IO ()
promptAddEx = undefined

promptRemoveEx :: Prompt -> String -> IO Bool
promptRemoveEx = undefined


{-
- prompt ==| ssh =|= exec =|= kill ==[ addW =|= addW2
-
-
-
-}

