
module Moonbase.Panel.Gtk.StrutProperties
  ( setStrutProperties
  , StrutProperties(..)
  , newStrutProp
  ) where

import Graphics.UI.Gtk
import Foreign
import Foreign.C.Types
import Unsafe.Coerce ( unsafeCoerce )

data StrutProperties = StrutProperties 
 { left        :: Int
 , right       :: Int
 , top         :: Int
 , bottom      :: Int
 , leftStartY  :: Int
 , leftEndY    :: Int
 , rightStartY :: Int
 , rightEndY   :: Int
 , leftStartX  :: Int
 , leftEndX    :: Int
 , rightStartX :: Int
 , rightEndX   :: Int }

newStrutProp :: StrutProperties
newStrutProp = StrutProperties 0 0 0 0 0 0 0 0 0 0 0 0



propertiesToArray :: StrutProperties -> [CLong]
propertiesToArray props = map (\pos -> fromIntegral $ pos props) positions
    where
      positions = [ left, right
                  , top, bottom
                  , leftStartY, leftEndY
                  , rightStartY, rightEndY
                  , leftStartX, leftEndX
                  , rightStartX, rightEndX ]


foreign import ccall "set_properties" c_set_properties :: Ptr Window -> Ptr CLong -> ()

setStrutProperties :: Window -> StrutProperties -> IO ()
setStrutProperties win props = do
    let ptrWin = unsafeCoerce win :: ForeignPtr Window
    ptrProps <- newArray $ propertiesToArray props
    withForeignPtr ptrWin $ \ptr -> return $ c_set_properties ptr ptrProps
