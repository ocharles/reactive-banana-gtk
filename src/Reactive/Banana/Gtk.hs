module Reactive.Banana.Gtk (module Reactive.Banana.Gtk, (<~)) where

import Reactive.Banana
import Reactive.Banana.Gtk.Internal
import Reactive.Banana.Frameworks
import Data.Monoid
import qualified Graphics.UI.Gtk as Gtk
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT, runReaderT, mapReaderT, withReaderT)
import Control.Monad.Trans.Writer.Strict (WriterT, execWriterT, mapWriterT, Writer)
import Data.Foldable (traverse_, for_)

type GtkApp = forall t. (Frameworks t) => Gtk Gtk.WidgetClass (Last Gtk.Widget) t ()

runGtk :: GtkApp -> IO ()
runGtk builder =
  do Gtk.initGUI
     network <-
       compile (do gtkWindow <- liftIO Gtk.windowNew
                   children <-
                     execWriterT
                       (runReaderT (unGtk builder)
                                   (Cast (pure . Gtk.toWidget)))
                   childrenChanged <- changes children
                   initialChildren <- initial children
                   liftIOLater
                     (do traverse_ (Gtk.containerAdd gtkWindow)
                                   (getLast initialChildren)
                         Gtk.widgetShow gtkWindow))
     actuate network
     Gtk.mainGUI
