module Moonbase.Util.Gtk.Widget.Prompt
        (
        ) where

import Control.Monad


import Moonbase.Util.Gtk.Color
import Moonbase.Util.Gtk

import Graphics.UI.Gtk

import System.IO.Unsafe

import Data.Maybe
import qualified Data.Map as M



type PromptComplFunc = (String -> IO [String])
type PromptExecFunc  = (String -> IO ())

data PromptExtension = PromptExtentsion String PromptComplFunc PromptExecFunc

data PromptTheme = PromptTheme
    { promptBackground :: HexColor
    , promptForeground :: HexColor
    , promptHeight     :: Int
    , promptPosition   :: Position
}


type Prompt = Window

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



maybeEntry :: Attr Prompt (Maybe Entry)
maybeEntry = unsafePerformIO $ objectCreateAttribute
{-# NOINLINE maybeEntry #-}

promptEntry :: ReadAttr Prompt Entry
promptEntry = readAttr $ \obj -> do
    mEntry <- get obj maybeEntry
    if isNothing mEntry
       then error $ "Could not get entry..."
       else return $ fromJust mEntry


maybeView :: Attr Prompt (Maybe TreeView)
maybeView = unsafePerformIO $ objectCreateAttribute
{-# NOINLINE maybeView #-}

promptView :: ReadAttr Prompt TreeView
promptView = readAttr $ \obj -> do
    mView <- get obj maybeView
    if isNothing mView
       then error $ "Could not get view..."
       else return $ fromJust mView


promptNew :: PromptTheme -> IO Prompt
promptNew theme = do

   scr <- check =<< screenGetDefault

   prompt  <- windowNew

   widgetSetName prompt "Prompt"

   windowSetScreen prompt scr
   windowSetKeepAbove prompt True
   windowSetTypeHint prompt WindowTypeHintPopupMenu

   widgetModifyBg prompt StateNormal (parseColor $ promptBackground theme)
   widgetModifyFg prompt StateNormal (parseColor $ promptForeground theme)

   setSize prompt

   _ <- on scr screenMonitorsChanged $ setSize prompt

   -- implement me

   return prompt

    
   where
       check (Just scr) = return $ scr
       check Nothing    = error $ "Could not open default screen"

       setSize          = setPromptSize (promptPosition theme) (promptHeight theme)


setPromptSize :: Position -> Int -> Window -> IO ()
setPromptSize pos height prompt = do
    scr      <- windowGetScreen prompt
    (mX, mY) <- getAbsoluteMousePosition scr
    mo       <- screenGetMonitorAtPoint scr mX mY

    moSelGeo@(Rectangle x y w h) <- screenGetMonitorGeometry scr mo

    windowSetDefaultSize prompt w height
    widgetSetSizeRequest prompt w height
    windowResize prompt w height

    moveWindow prompt pos height moSelGeo

    _ <- on prompt realize $ setWindowStruts prompt pos height moSelGeo
    
    isRealized <- widgetGetRealized prompt
    when isRealized $ setWindowStruts prompt pos height moSelGeo

    

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

