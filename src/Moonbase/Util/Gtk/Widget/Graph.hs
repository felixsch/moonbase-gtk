module Moonbase.Util.Gtk.Widget.Graph
 ( GraphStyle(..)
 , GraphDirection(..)
 , GraphConfig(..)
 , GraphHistory
 , Graph
 , graphNew
 ) where

import Control.Monad

import Moonbase.Core
import Moonbase.Util.Gtk.Color

import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Maybe

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
  , graphAxes    :: Maybe HexColor
  , graphSeqColors   :: M.Map String HexColor
  , graphBorder      :: Maybe (Int, HexColor)
  , graphBackground  :: Maybe HexColor
  }


type GraphHistory = M.Map String [Double]


emptyHistory :: GraphHistory
emptyHistory = M.empty


type Graph = DrawingArea


maybeHistory :: IO (Attr Graph (Maybe GraphHistory))
maybeHistory = objectCreateAttribute

maybeConfig :: IO (Attr Graph (Maybe GraphConfig))
maybeConfig = objectCreateAttribute


graphHistory :: Attr Graph GraphHistory
graphHistory = newAttr getHistory setHistory
    where
        getHistory object         = do
            attr <- maybeHistory
            mHistory <- get object attr
            return $ fromMaybe emptyHistory mHistory
        
        setHistory object history = do
            attr <- maybeHistory
            set object [attr := Just history]


graphConfig :: Attr Graph GraphConfig
graphConfig = newAttr getConfig setConfig
    where
        getConfig object = do
            attr <- maybeConfig
            mConfig <- get object attr
            case mConfig of
                Nothing -> error "Could not load Config"
                Just c  -> return c
        setConfig object config = do
            attr <- maybeConfig
            set object [attr := Just config]


graphNew :: GraphConfig -> IO Graph
graphNew config = do
    graph <- drawingAreaNew

    set graph [graphConfig := config]
    set graph [graphHistory := emptyHistory]

    _ <- on graph exposeEvent $ tryEvent $ io $ do
        conf  <- get graph graphConfig
        history <- get graph graphHistory
        
        drawGraph graph conf history
    return graph


drawGraph :: Graph -> GraphConfig -> GraphHistory -> IO ()
drawGraph graph conf hist = do

    area <- widgetGetWindow graph

    case area of 
      Nothing  -> return ()
      Just win -> do
        w <- drawWindowGetWidth win
        h <- drawWindowGetHeight win
        renderWithDrawWindow win $ render w h

    where
        render w h = do
            when hasBackground $ renderBackground w h (fromJust $ graphBackground conf)
            when hasBorder     $ renderBorder     w h (graphPadding conf) (fromJust $ graphBorder conf)
            when hasAxes       $ renderAxes       w h (graphPadding conf) oaMin oaMax (fromJust $ graphAxes conf)

            case graphStyle conf of
                LineGraph       -> renderLineGraph      w h (graphPadding conf) (graphSeqColors conf) hist
                AreaGraph tran  -> renderAreaGraph tran w h (graphPadding conf) (graphSeqColors conf) hist

        hasBackground = isJust $ graphBackground conf
        hasBorder     = isJust $ graphBorder     conf
        hasAxes       = isJust $ graphAxes       conf

        values        = concat $ M.elems hist
        oaMax         = maximum values
        oaMin         = minimum values







renderBackground :: Int -> Int -> HexColor -> Render ()
renderBackground w h c = setSourceRGB r g b >> rectangle 0 0 (fromIntegral w) (fromIntegral h) >> paint
    where
        (r, g, b) = parseColor' c

renderBorder :: Int -> Int -> Int -> (Int, HexColor) -> Render ()
renderBorder w h padding (wid, c) = setLineWidth (fromIntegral wid) >> rectangle pad pad w' h' >> paint
    where
        (r, g, b) = parseColor' c
        w'        = fromIntegral w - pad
        h'        = fromIntegral h - pad
        pad       = fromIntegral padding

renderAxes :: Int -> Int -> Int -> Double -> Double -> HexColor -> Render ()
renderAxes w h padding min max c = setLineWidth 1 >> setSourceRGB r g b
    >> moveTo x1 y1 >> lineTo x1 pad >> stroke
    >> moveTo x1 y1 >> lineTo pad y1 >> stroke
    where
        (r, g, b) = parseColor' c
        pad       = fromIntegral padding
        x1        = fromIntegral w - pad
        y1        = fromIntegral h - pad

renderLineGraph :: Int -> Int -> Int -> M.Map String HexColor -> GraphHistory -> Render ()
renderLineGraph = undefined

renderAreaGraph :: Bool -> Int -> Int -> Int -> M.Map String HexColor -> GraphHistory -> Render ()
renderAreaGraph = undefined
            
        




    


    



    
    



