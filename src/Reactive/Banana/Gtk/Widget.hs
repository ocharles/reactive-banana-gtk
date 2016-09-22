module Reactive.Banana.Gtk.Widget where

import Data.Foldable
import Data.Monoid
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Gtk.Internal
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Graphics.UI.Gtk as Gtk

data WidgetSignals =
  WidgetSignals

class IsWidget a  where
  widgetSignals :: a -> WidgetSignals

listenWidgetSignals
  :: Gtk.WidgetClass widget
  => widget -> MomentIO WidgetSignals
listenWidgetSignals _ = return WidgetSignals
