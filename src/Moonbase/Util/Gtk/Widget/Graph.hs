module Moonbase.Util.Gtk.Widget.Graph
 ( GraphStyle(..)
 , GraphDirection(..)
 , GraphConfig(..)
 , GraphHistory
 , Graph
 , graphNew
 , graphConfig
 , graphHistory
 , pollingGraphNew
 , defaultGraphConfig
 ) where

import Control.Monad
import Control.Concurrent

import Control.Exception

import qualified Moonbase.Theme as Moon
import Moonbase.Util.Gtk

import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Maybe

import System.IO.Unsafe
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo


data GraphStyle = LineGraph
                | AreaGraph Bool

data GraphDirection = GraphRightToLeft
                    | GraphLeftToRight


data GraphConfig = GraphConfig 
  { graphPadding     :: Int
  , graphDirection   :: GraphDirection
  , graphStyle       :: GraphStyle
  , graphWidth       :: Int
  , graphColor       :: Moon.Color
  , graphBorder      :: Maybe (Int, Moon.Color)
  , graphBackground  :: Moon.Color
  }


defaultGraphConfig :: GraphConfig
defaultGraphConfig = GraphConfig
  { graphPadding    = 4
  , graphDirection  = GraphLeftToRight
  , graphStyle      = LineGraph
  , graphWidth      = 128
  , graphColor      = "#ff0000"
  , graphBorder     = Just (1, "#0000ff")
  , graphBackground = "#24ff25"
  }

type GraphHistory = S.Seq Double


emptyHistory :: GraphHistory
emptyHistory = S.empty


type Graph = DrawingArea


maybeHistory :: Attr Graph (Maybe GraphHistory)
maybeHistory = unsafePerformIO $ objectCreateAttribute
{-# NOINLINE maybeHistory #-}

maybeConfig :: Attr Graph (Maybe GraphConfig)
maybeConfig = unsafePerformIO $ objectCreateAttribute
{-# NOINLINE maybeConfig #-}

graphHistory :: Attr Graph GraphHistory
graphHistory = newAttr getHistory setHistory
    where
        getHistory object         = do
            mHistory <- get object maybeHistory
            when (isNothing mHistory) $ putStrLn "Could not get history..."
            return $ fromMaybe emptyHistory mHistory
        
        setHistory object history = do
            set object [maybeHistory := Just history]


graphConfig :: Attr Graph GraphConfig
graphConfig = newAttr getConfig setConfig
    where
        getConfig object = do
            mConfig <- get object maybeConfig
            case mConfig of
                Nothing -> error "Could not load graph config"
                Just c  -> return c
        setConfig object config = do
            set object [maybeConfig := Just config]

graphNew :: GraphConfig -> IO Graph
graphNew config = do
    graph <- drawingAreaNew

    set graph [maybeConfig := (Just config)]
    set graph [maybeHistory := (Just emptyHistory)]
    
    widgetSetSizeRequest graph (graphWidth config) (-1)
    _ <- on graph draw $ do
        history <- liftIO $ get graph graphHistory 
        (w,h)   <- liftIO $ getSize graph

        let (_, needed) = S.splitAt (S.length history - nSize w) history
        drawGraph w h config needed
        liftIO $ set graph [graphHistory := needed]

    return graph

 where
    nSize w = w - (graphPadding config * 2)

pollingGraphNew :: GraphConfig -> Int -> (Graph -> IO ()) -> IO Graph
pollingGraphNew conf ms f = do
    graph <- graphNew conf

    _ <- on graph realize $
        void $ forkIO $ forever $ do
            f graph
            h <- get graph graphHistory
            postGUIAsync $ widgetQueueDraw graph
            threadDelay $ ms * 1000
    return graph

getSize :: Graph -> IO (Int, Int)
getSize graph = do
    area <- widgetGetWindow graph

    case area of
         Nothing  -> return (0,0)
         Just win -> do
            w <- drawWindowGetWidth win
            h <- drawWindowGetHeight win
            return (w, h)


drawGraph :: Int -> Int -> GraphConfig -> GraphHistory -> Render ()
drawGraph w h conf hist = do
    renderBackground w h (graphBackground conf)
    when hasBorder     $ renderBorder     w h (graphPadding conf) (fromJust $ graphBorder conf)

    case graphStyle conf of
        LineGraph       -> renderLineGraph      w h (graphPadding conf) (graphColor conf) hist
        AreaGraph tran  -> renderAreaGraph tran w h (graphPadding conf) (graphColor conf) hist
    where
        hasBorder     = isJust $ graphBorder     conf


renderBackground :: Int -> Int -> Moon.Color -> Render ()
renderBackground w h c = setSourceRGB r g b >> rectangle 0 0 (fromIntegral w) (fromIntegral h) >> fill
    where
        (r, g, b) = parseColor' c

renderBorder :: Int -> Int -> Int -> (Int, Moon.Color) -> Render ()
renderBorder w h padding (wid, c) = setSourceRGB r g b >> setLineWidth wid' >> moveTo j j >> rectangle j j w' h' >> stroke
    where
        (r, g, b) = parseColor' c
        w'        = fromIntegral w - 2*j
        h'        = fromIntegral h - 2*j
        pad       = fromIntegral padding
        wid'      = fromIntegral wid
        j         = pad - wid' / 2


renderLineGraph :: Int -> Int -> Int -> Moon.Color -> GraphHistory -> Render ()
renderLineGraph w h padding color hist = do
   setLineWidth 1
   setSourceRGB r g b
   void $ loopR hist $ \index value -> do
       let x = xm - fI index
       let y = y0 - (ye * value)
       moveTo x y0
       lineTo x y
       stroke
     where

      pad     = fI padding
      xm      = fI w - pad
      y0      = fI h - pad
      ye      = fI h - (2 * pad)


      (r,g,b) = parseColor' color

      fI      = fromIntegral

renderAreaGraph :: Bool -> Int -> Int -> Int -> Moon.Color -> GraphHistory -> Render ()
renderAreaGraph trans w h pad' color hist = do
    setLineWidth 1
    setTrans trans
    moveTo pad y0
    lineTo xm  y0
    void $ loopR hist $ \index value -> do
       let x = xm - fI index
       let y = y0 - (ye * value)
       lineTo x y 

    paint

    when trans $ do
        setLineWidth 2
        setSourceRGB r g b
        let (first, hist') = S.splitAt 1 hist
        moveTo xm (y0 - (ye * (check first)))
        void $ loopR hist' $ \index value -> do
            let x = xm - fI index
            let y = y0 - (ye * value)
            lineTo x y
        stroke
 where

     pad  = fI pad'
     y0   = fI h - pad
     ye   = fI h - (2 * pad)
     xm   = fI w - pad

     setTrans True  = setSourceRGBA r g b 125
     setTrans False = setSourceRGB r g b

     (r, g, b) = parseColor' color

     fI = fromIntegral

     check x
       | S.null x = 0.0
       | otherwise = S.index x 1
           

loopR :: (Monad m) => S.Seq Double -> (Int -> Double -> m a) -> m [a]
loopR s f = sequence $ loop' 0 $ S.viewr s
  where
      loop' i (xs S.:> x) = (f i x) : (loop' (i+1) $ S.viewr xs)
      loop' _ S.EmptyR    = []
        




    


    



    
    



