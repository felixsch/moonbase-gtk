module Moonbase.Util.Gtk.Widget.Prompt
        ( PromptComplFunc
        , PromptExecFunc
        , PromptExtension(..)
        , PromptTheme(..)
        , Prompt(..)
        , promptWidgets
        , promptTheme
        , promptEntry
        , promptNew
        , promptShow
        , promptHide
        ) where

import Control.Monad
import Control.Monad.Reader


import Moonbase.Theme
import Moonbase.Util.Gtk
import Moonbase.Util.Gtk

import qualified Graphics.UI.Gtk as Gtk

import System.IO.Unsafe

import Data.Maybe
import qualified Data.Map as M



type PromptComplFunc = (String -> IO [String])
type PromptExecFunc  = (String -> IO ())

data PromptExtension = PromptExtentsion String PromptComplFunc PromptExecFunc

data PromptTheme = PromptTheme
    { promptBackground :: Color
    , promptForeground :: Color
    , promptHeight     :: Int
    , promptPosition   :: Position
}


type Prompt = Gtk.Window

maybeWidgets :: Gtk.Attr Prompt (Maybe (M.Map String Gtk.Widget))
maybeWidgets = unsafePerformIO $ Gtk.objectCreateAttribute
{-# NOINLINE maybeWidgets #-}


promptWidgets :: Gtk.Attr Prompt (M.Map String Gtk.Widget)
promptWidgets = Gtk.newAttr getWidgets setWidgets
    where
        getWidgets obj = do
            mWidgets <- Gtk.get obj maybeWidgets
            return $ fromMaybe M.empty mWidgets

        setWidgets obj widgets = do
            Gtk.set obj [maybeWidgets Gtk.:= Just widgets]

maybeExtensions :: Gtk.Attr Prompt (Maybe (M.Map String PromptExtension))
maybeExtensions = unsafePerformIO $ Gtk.objectCreateAttribute
{-# NOINLINE maybeExtensions #-}

promptExtensions :: Gtk.Attr Prompt (M.Map String PromptExtension)
promptExtensions = Gtk.newAttr getExts setExts
    where
        getExts obj = do
            mExts <- Gtk.get obj maybeExtensions
            return $ fromMaybe M.empty mExts
        setExts obj exts = Gtk.set obj [maybeExtensions Gtk.:= Just exts]

maybeTheme :: Gtk.Attr Prompt (Maybe PromptTheme)
maybeTheme = unsafePerformIO $ Gtk.objectCreateAttribute
{-# NOINLINE maybeTheme #-}

promptTheme :: Gtk.ReadAttr Prompt PromptTheme
promptTheme = Gtk.readAttr $ \obj -> do
    mTheme <- Gtk.get obj maybeTheme
    if isNothing mTheme
       then error $ "Could not load theme..."
       else return $ fromJust mTheme



maybeEntry :: Gtk.Attr Prompt (Maybe Gtk.Entry)
maybeEntry = unsafePerformIO $ Gtk.objectCreateAttribute
{-# NOINLINE maybeEntry #-}

promptEntry :: Gtk.ReadAttr Prompt Gtk.Entry
promptEntry = Gtk.readAttr $ \obj -> do
    mEntry <- Gtk.get obj maybeEntry
    if isNothing mEntry
       then error $ "Could not get entry..."
       else return $ fromJust mEntry


maybeView :: Gtk.Attr Prompt (Maybe Gtk.TreeView)
maybeView = unsafePerformIO $ Gtk.objectCreateAttribute
{-# NOINLINE maybeView #-}

promptView :: Gtk.ReadAttr Prompt Gtk.TreeView
promptView = Gtk.readAttr $ \obj -> do
    mView <- Gtk.get obj maybeView
    if isNothing mView
       then error $ "Could not get view..."
       else return $ fromJust mView


promptNew :: PromptTheme -> IO Prompt
promptNew theme = do

   scr <- check =<< Gtk.screenGetDefault

   prompt  <- Gtk.windowNew

   Gtk.widgetSetName prompt "Prompt"

   Gtk.set prompt [ Gtk.windowSkipTaskbarHint Gtk.:= True
                  , Gtk.windowSkipPagerHint Gtk.:= True
                  , Gtk.windowAcceptFocus Gtk.:= True
                  , Gtk.windowDecorated Gtk.:= False
                  , Gtk.windowHasResizeGrip Gtk.:= False
                  , Gtk.windowResizable Gtk.:= False 
                  , Gtk.windowRole Gtk.:= "MoonbasePrompt" ]

   Gtk.windowSetScreen prompt scr
   Gtk.windowSetKeepAbove prompt True
   Gtk.windowSetTypeHint prompt Gtk.WindowTypeHintMenu


   Gtk.widgetModifyBg prompt Gtk.StateNormal (parseColor $ promptBackground theme)
   Gtk.widgetModifyFg prompt Gtk.StateNormal (parseColor $ promptForeground theme)

   setSize prompt

   _ <- Gtk.on scr Gtk.screenMonitorsChanged $ setSize prompt

   view  <- Gtk.treeViewNew
   entry <- Gtk.entryNew
   Gtk.entrySetHasFrame entry False

   setStyle entry "promptEntry" [ ("background", color_ $ promptBackground theme) ]

   typLabel <- Gtk.labelNew (Just ">>")
   matchesLabel <- Gtk.labelNew (Just "0 matches")
   
   Gtk.labelSetMarkup typLabel "<b><span color=\"#ff0000\">::</span></b>"
   
   hbox <- Gtk.hBoxNew False 1
   vbox <- Gtk.vBoxNew False 1

   Gtk.boxPackStart hbox typLabel Gtk.PackNatural 1
   Gtk.boxPackStart hbox entry Gtk.PackGrow 1
   Gtk.boxPackStart hbox matchesLabel Gtk.PackNatural 1

   Gtk.containerAdd vbox view
   Gtk.containerAdd vbox hbox

   Gtk.containerAdd prompt vbox

   Gtk.set prompt [ maybeTheme Gtk.:= Just theme
                  , maybeEntry Gtk.:= Just entry
                  , maybeView  Gtk.:= Just view ]

   -- Add escape button to close window
   _ <- Gtk.on prompt Gtk.keyPressEvent $ Gtk.tryEvent $ do
       "Escape" <- Gtk.eventKeyName
       time     <- Gtk.eventTime
       liftIO $ promptHide time prompt

   return prompt

    
   where
       check (Just scr) = return $ scr
       check Nothing    = error $ "Could not open default screen"

       setSize          = setPromptSize (promptPosition theme) (promptHeight theme)


foreachState :: (Gtk.StateType -> Gtk.Color -> IO ()) -> Gtk.Color -> IO ()
foreachState f c = mapM_ (\s -> f s c) [Gtk.StateNormal, Gtk.StateActive]

setPromptSize :: Position -> Int -> Gtk.Window -> IO ()
setPromptSize pos height prompt = do
    scr      <- Gtk.windowGetScreen prompt
    (mX, mY) <- getAbsoluteMousePosition scr
    mo       <- Gtk.screenGetMonitorAtPoint scr mX mY

    moSelGeo@(Gtk.Rectangle x y w h) <- Gtk.screenGetMonitorGeometry scr mo

    Gtk.windowSetDefaultSize prompt w height
    Gtk.widgetSetSizeRequest prompt w height
    Gtk.windowResize prompt w height

    moveWindow prompt pos moSelGeo
    

promptShow :: Prompt -> IO ()
promptShow pr = ioasync $ do

        Gtk.widgetShowAll pr 

        entry <- Gtk.get pr promptEntry
        mWin  <- Gtk.widgetGetWindow pr

        Gtk.windowPresent pr
        Gtk.widgetGrabFocus entry

        case mWin of
             Nothing  -> return ()
             Just win -> do
                 Gtk.keyboardGrab win True Gtk.currentTime
                 Gtk.pointerGrab win True [Gtk.AllEventsMask] noWindow Nothing Gtk.currentTime
                 return ()

noWindow :: Maybe Gtk.DrawWindow
noWindow = Nothing

        

promptHide :: Gtk.TimeStamp -> Prompt -> IO ()
promptHide time pr = ioasync $ do
       isPointerGrabbed <- Gtk.pointerIsGrabbed

       when isPointerGrabbed $ Gtk.pointerUngrab time
       Gtk.keyboardUngrab time

       Gtk.widgetHide pr

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

