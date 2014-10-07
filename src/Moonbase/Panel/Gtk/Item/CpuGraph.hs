module Moonbase.Panel.Gtk.Item.CpuGraph
    ( cpuGraph
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad (forever, void)

import System.IO


import qualified Data.Sequence as S
import Data.Time.Format
import Data.Time.LocalTime

import Graphics.UI.Gtk

import Moonbase.Core
import Moonbase.Panel.Gtk
import Moonbase.Util.Gtk
import Moonbase.Util.Gtk.Widget.Graph


data ItemCpuGraph = ItemCpuGraph Int GraphConfig (Maybe Graph) 


instance PanelItem ItemCpuGraph where
    getWidget (ItemCpuGraph _ _ (Just g)) = toWidget g

    initItem (ItemCpuGraph poll conf _)   = do
        graph <- io $ pollingGraphNew conf poll cpuFetchInfo

        return (ItemCpuGraph poll conf (Just graph), toWidget graph)

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
    set graph [graphHistory :~ (S.|> t)]
  where
      readStat = do
          hdl  <- openFile "/proc/stat" ReadMode
          line <- hGetLine hdl
          hClose hdl
          return $ parse line    
      parse = map read . tail . words

cpuGraph :: Int -> GraphConfig -> Item
cpuGraph poll config = Item "cpuGraph" PackNatural $ ItemCpuGraph poll config Nothing




