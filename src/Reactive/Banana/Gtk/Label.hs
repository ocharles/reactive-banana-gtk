{-# LANGUAGE UndecidableInstances #-}

module Reactive.Banana.Gtk.Label
  ( -- * Labels
    Label, label,
    -- * Implementation Details
    IsLabel
  ) where

import Data.String
import Data.Foldable
import Data.Monoid
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Gtk.Internal
import Reactive.Banana.Gtk.Widget
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Graphics.UI.Gtk as Gtk

data Label =
  Label {labelWidget :: Gtk.Label
        ,labelLabelSignals :: LabelSignals
        ,labelWidgetSignals :: WidgetSignals}

data LabelSignals = LabelSignals {}

class IsLabel a where
  labelSignals :: a -> LabelSignals

instance IsLabel Label where
  labelSignals = labelLabelSignals

instance IsWidget Label where
  widgetSignals = labelWidgetSignals

label :: (Monoid widget, c Gtk.Label)
       => Attribute Gtk.Label ()
       -> Behavior String
       -> Gtk c widget Label
label attributes contents =
  do widget <-
       liftIO (unsafeInterleaveIO (Gtk.labelNew (Nothing :: Maybe String)))
     rb (applyAttributes attributes widget)
     rb (do initialContents <- valueBLater contents
            liftIOLater
              (do Gtk.labelSetText widget initialContents
                  Gtk.widgetShow widget))
     tellCast widget
     rb (Label widget <$> listenLabelSignals widget <*>
         listenWidgetSignals widget)

listenLabelSignals
  :: (Gtk.LabelClass label)
  => label -> MomentIO LabelSignals
listenLabelSignals widget =
  do return LabelSignals {}

instance (a ~ Label, c Gtk.Label, Monoid widget) => IsString (Gtk c widget a) where
  fromString = label (return ()) . pure
