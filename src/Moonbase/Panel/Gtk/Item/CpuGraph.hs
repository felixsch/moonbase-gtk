module Moonbase.Panel.Gtk.Item.CpuGraph
    ( cpuGraph
    , cpuGraphWith
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad (forever, void)

import System.IO


import qualified Data.Sequence as S
import Data.Time.Format
import Data.Time.LocalTime

import qualified Graphics.UI.Gtk as Gtk

import Moonbase
import Moonbase.Theme
import Moonbase.Item
import Moonbase.Panel.Gtk
import Moonbase.Util.Gtk
import Moonbase.Util.Gtk.Widget.Graph



cpuGraphWith :: GraphConfig -> Int -> GtkPanelItem
cpuGraphWith config poll = item $ do
    graph <- io $ pollingGraphNew config poll cpuFetchInfo
    return (Gtk.toWidget graph, Gtk.PackNatural)

cpuGraph :: Int -> GtkPanelItem 
cpuGraph poll = item $ do
    theme <- moon $ getTheme
    moon $ push (Info $ "bg = " ++ bg theme)
    graph <- io $ pollingGraphNew (genConfig theme) poll cpuFetchInfo
    return (Gtk.toWidget graph, Gtk.PackNatural)
  where
      genConfig theme = GraphConfig 
        { graphPadding    = 4
        , graphDirection  = GraphLeftToRight
        , graphStyle      = LineGraph
        , graphWidth      = 120
        , graphColor      = activeC theme
        , graphBorder     = Nothing
        , graphBackground = bg theme }

truncVal :: Double -> Double
truncVal v
  | isNaN v || v < 0.0 = 0.0
  | otherwise = v

cpuFetchInfo :: Graph -> IO ()
cpuFetchInfo graph = do
    a <- readStat
    threadDelay 50000
    b <- readStat

    let dif = zipWith (-) b a
        tot = foldr (+) 0 dif
        pct = map (/ tot) dif
        user = foldr (+) 0 $ take 2 pct
        system = pct !! 2
        t = user + system
    Gtk.set graph [graphHistory Gtk.:~ (S.|> t)]
  where
      readStat = do
          hdl  <- openFile "/proc/stat" ReadMode
          line <- hGetLine hdl
          hClose hdl
          return $ parse line    
      parse = map read . tail . words

