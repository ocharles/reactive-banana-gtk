module Reactive.Banana.Gtk.Button
  ( -- * Buttons
    Button, button,
    -- ** Signals
    clicked,
    -- ** Attributes
    useStock,
    -- * Implementation Details
    IsButton
  ) where

import Data.Foldable
import Data.Monoid
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Gtk.Container
import Reactive.Banana.Gtk.Internal
import Reactive.Banana.Gtk.Widget
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Graphics.UI.Gtk as Gtk

data Button t =
  Button {buttonWidget :: Gtk.Button
         ,buttonButtonSignals :: ButtonSignals t
         ,buttonContainerSignals :: ContainerSignals t
         ,buttonWidgetSignals :: WidgetSignals t}

data ButtonSignals t = ButtonSignals { buttonClicked :: Event t () }

class IsButton a where
  buttonSignals :: a t -> ButtonSignals t

instance IsButton Button where
  buttonSignals = buttonButtonSignals

instance IsContainer Button where
  containerSignals = buttonContainerSignals

instance IsWidget Button where
  widgetSignals = buttonWidgetSignals

clicked :: IsButton button
        => button t -> Event t ()
clicked button =
  buttonClicked (buttonSignals button)

button :: (Monoid widget,Frameworks t, c Gtk.Button)
       => Attribute Gtk.Button t ()
       -> Gtk Gtk.WidgetClass (Last Gtk.Widget) t ()
       -> Gtk c widget t (Button t)
button attributes mkChildren =
  do widget <-
       liftIO (unsafeInterleaveIO Gtk.buttonNew)
     rb (applyAttributes attributes widget)
     children <-
       bin (pure . Gtk.toWidget) mkChildren
     rb (do initialChildren <- initial children
            liftIOLater
              (do traverse_ (Gtk.containerAdd widget)
                            (getLast initialChildren)
                  Gtk.widgetShow widget))
     tellCast widget
     rb (Button widget <$> listenButtonSignals widget <*>
         listenContainerSignals widget <*>
         listenWidgetSignals widget)

listenButtonSignals
  :: (Gtk.ButtonClass button,Frameworks t)
  => button -> Moment t (ButtonSignals t)
listenButtonSignals widget =
  do (buttonClicked,fireClick) <- newEvent
     liftIOLater
       (do _ <-
             Gtk.on widget Gtk.buttonActivated (fireClick ())
           return ())
     return ButtonSignals {..}

-- TODO Deprecated, so I should remove this
useStock :: Gtk.ButtonClass self => Gtk.Attr self Bool
useStock = Gtk.buttonUseStock
