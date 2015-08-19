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

data Label t =
  Label {labelWidget :: Gtk.Label
        ,labelLabelSignals :: LabelSignals t
        ,labelWidgetSignals :: WidgetSignals t}

data LabelSignals t = LabelSignals {}

class IsLabel a where
  labelSignals :: a t -> LabelSignals t

instance IsLabel Label where
  labelSignals = labelLabelSignals

instance IsWidget Label where
  widgetSignals = labelWidgetSignals

label :: (Monoid widget,Frameworks t, c Gtk.Label)
       => Attribute Gtk.Label t ()
       -> Behavior t String
       -> Gtk c widget t (Label t)
label attributes contents =
  do widget <-
       liftIO (unsafeInterleaveIO (Gtk.labelNew (Nothing :: Maybe String)))
     rb (applyAttributes attributes widget)
     rb (do initialContents <- initial contents
            liftIOLater
              (do Gtk.labelSetText widget initialContents
                  Gtk.widgetShow widget))
     tellCast widget
     rb (Label widget <$> listenLabelSignals widget <*>
         listenWidgetSignals widget)

listenLabelSignals
  :: (Gtk.LabelClass label,Frameworks t)
  => label -> Moment t (LabelSignals t)
listenLabelSignals widget =
  do return LabelSignals {}

instance (a ~ Label t, c Gtk.Label, Frameworks t, Monoid widget) => IsString (Gtk c widget t a) where
  fromString = label (return ()) . pure
