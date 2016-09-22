{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Functor.Contravariant
import Data.Bifunctor (second)
import Data.IORef
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Monoid (Last(..))
import Control.Monad (void)
import Control.Monad.Fix
import Data.String
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Writer (MonadWriter, tell)
import Control.Applicative
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, mapReaderT, withReaderT)
import Control.Monad.Trans.Writer.Strict (WriterT, execWriterT, mapWriterT)
import Graphics.UI.Gtk
       (initGUI, mainGUI, Widget, windowNew, containerAdd, buttonNew,
        castToWidget, widgetShow, labelNew, labelSetText, on,
        buttonActivated, hPanedNew, vPanedNew, castToPaned,
        scrolledWindowNew, viewportNew, drawingAreaNew, checkButtonNew,
        hBoxNew, vBoxNew, castToBox, AttrOp((:=)), linkButtonNew,
        containerForall, containerRemove, radioButtonNewFromWidget,
        radioButtonNew, toggleButtonNew, volumeButtonNew, levelBarNew,
        spinnerNew, notebookNew, menuBarNew, menuItemNew,
        menuItemSetSubmenu, menuNew, frameNew, frameSetLabelWidget,
        entryNew)
import Reactive.Banana (Behavior, Event, stepper, valueBLater)
import Data.Foldable (traverse_, for_)
import Reactive.Banana.Frameworks
       (MomentIO, compile, actuate, reactimate', changes,
        liftIOLater, newEvent)
import Unsafe.Coerce (unsafeCoerce)
import qualified Graphics.UI.Gtk as GTK

instance IsString a => IsString (Behavior a) where
  fromString = pure . fromString

data Cast c b where
  Cast :: (forall a. c a => (a -> b)) -> Cast c b

tellCast x =
  do Cast cast <- ask
     tell (pure (cast x))

bin :: (Monoid widget2) => (forall a. c1 a => a -> widget1) -> Gtk c1 widget1 x -> Gtk c2 widget2 (Behavior widget1)
bin cast mkChildren =
  rb (execWriterT
        (runReaderT (unGtk mkChildren)
                    (Cast cast)))

newtype Gtk c widget a =
  Gtk {unGtk :: ReaderT (Cast c widget) (WriterT (Behavior widget) MomentIO) a}

instance (a ~ (), Monoid widget) => Monoid (Gtk c widget a) where
  mempty = return ()
  mappend = (>>)

instance Functor (Gtk c widget) where
  fmap f (Gtk m) = Gtk (fmap f m)

instance Monoid widget => Applicative (Gtk c widget) where
  pure a = Gtk (pure a)
  Gtk f <*> Gtk a = Gtk (f <*> a)

instance Monoid widget => Monad (Gtk c widget) where
  return a = Gtk (pure a)
  Gtk a >>= f = Gtk (a >>= unGtk . f)

instance Monoid widget => MonadWriter (Behavior widget) (Gtk c widget) where
  tell x = Gtk (tell x)

instance Monoid widget => MonadReader (Cast c widget) (Gtk c widget) where
  ask = Gtk ask

instance Monoid widget => MonadFix (Gtk c widget) where
  mfix f = Gtk (mfix (unGtk . f))

instance Monoid widget => MonadIO (Gtk c widget) where
  liftIO io = Gtk (liftIO io)

instance Monoid a => Monoid (Behavior a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance (a ~ (), Monoid widget, c GTK.Label) => IsString (Gtk c widget a) where
  fromString = label . pure

#if !(MIN_VERSION_base(4,9,0))
instance Foldable Last where
  foldr f b (Last (Just a)) = f a b
  foldr _ b _ = b
#endif

rb :: Monoid widget => MomentIO a -> Gtk c widget a
rb m = Gtk (lift (lift m))

runGtk :: GtkApp -> IO ()
runGtk builder =
  do initGUI
     network <-
       compile (do gtkWindow <- liftIO windowNew
                   children <-
                     execWriterT (runReaderT (unGtk builder) (Cast (pure . GTK.toWidget)))
                   childrenChanged <- changes children
                   -- reactimate' (fmap (fmap (traverse_ (containerAdd gtkWindow))) childrenChanged)
                   initialChildren <- valueBLater children
                   liftIOLater
                     (do traverse_ (containerAdd gtkWindow) initialChildren
                         widgetShow gtkWindow))
     actuate network
     mainGUI

label :: (Monoid widget, c GTK.Label)
      => Behavior String -> Gtk c widget ()
label l =
  do widget <-
       liftIO (labelNew (Nothing :: Maybe String))
     rb (do initialContents <- valueBLater l
            liftIOLater
              (do labelSetText widget initialContents
                  widgetShow widget)
            labelChanged <- changes l
            reactimate' (fmap (fmap (labelSetText widget)) labelChanged))
     tellCast widget

button :: (Monoid widget, c GTK.Button)
       => Gtk GTK.WidgetClass (Last GTK.Widget) ()
       -> Gtk c widget (Event ())
button mkChildren =
  do widget <- liftIO buttonNew
     (clicked,fireClick) <- rb newEvent
     rb (liftIOLater
           (do _ <-
                 on widget buttonActivated (fireClick ())
               return ()))
     children <- bin (pure . GTK.toWidget) mkChildren
     rb (do initialChildren <- valueBLater children
            liftIOLater
              (do traverse_ (containerAdd widget) initialChildren
                  widgetShow widget))
     tellCast widget
     return clicked

checkButton :: (Monoid widget, c GTK.CheckButton)
            => Gtk GTK.WidgetClass (Last GTK.Widget) ()
            -> Gtk c widget (Event ())
checkButton mkChildren =
  do widget <- liftIO checkButtonNew
     (clicked,fireClick) <- rb newEvent
     rb (liftIOLater
           (do _ <-
                 on widget buttonActivated (fireClick ())
               return ()))
     children <- bin (pure . GTK.toWidget) mkChildren
     rb (do initialChildren <- valueBLater children
            liftIOLater
              (do traverse_ (containerAdd widget) initialChildren
                  widgetShow widget))
     tellCast widget
     return clicked

scrolled
  :: (Monoid widget, c GTK.ScrolledWindow)
  => Gtk GTK.WidgetClass (Last GTK.Widget) () -> Gtk c widget ()
scrolled mkChildren =
  do widget <-
       liftIO (scrolledWindowNew Nothing Nothing)
     children <-
       bin (pure . GTK.toWidget) mkChildren
     rb (do initialChildren <- valueBLater children
            liftIOLater
              (do traverse_ (containerAdd widget) initialChildren
                  widgetShow widget))
     tellCast widget

viewport
  :: (Monoid widget,c GTK.Viewport)
  => Gtk GTK.WidgetClass (Last GTK.Widget) () -> Gtk c widget ()
viewport mkChildren =
  do adjustment <-
       liftIO (fmap unsafeCoerce (newForeignPtr_ nullPtr)) -- gtk2hs #122
     widget <-
       liftIO (viewportNew adjustment adjustment)
     children <-
       bin (pure . GTK.toWidget) mkChildren
     rb (do initialChildren <- valueBLater children
            liftIOLater
              (do traverse_ (containerAdd widget) initialChildren
                  widgetShow widget))
     tellCast widget

data Orientation
  = Horizontal
  | Vertical

paned :: (Monoid widget, c GTK.Paned)
      => Orientation
      -> Gtk GTK.WidgetClass (Last GTK.Widget) ()
      -> Gtk GTK.WidgetClass (Last GTK.Widget) ()
      -> Gtk c widget ()
paned orientation left right =
  do widget <-
       liftIO (case orientation of
                 Horizontal ->
                   fmap castToPaned hPanedNew
                 Vertical ->
                   fmap castToPaned vPanedNew)
     for_ [left,right]
          (\mkSide ->
             do children <-
                  bin (pure . GTK.toWidget) mkSide
                rb (do initialChildren <- valueBLater children
                       liftIOLater
                         (do traverse_ (containerAdd widget) initialChildren
                             widgetShow widget)))
     tellCast widget

defaultPacking = GTK.PackGrow

data BoxPack a =
  BoxPack GTK.Packing
          a
  deriving (Functor)

packing :: Functor f
        => GTK.Packing
        -> Gtk c (f widget) ()
        -> Gtk c (f (BoxPack widget)) ()
packing packing (Gtk m) =
  Gtk (withReaderT
         (\(Cast f) ->
            Cast (\a ->
                    fmap (\(BoxPack _ x) -> x)
                         (f a)))
         (mapReaderT (mapWriterT (fmap (second (fmap (fmap (BoxPack packing))))))
                     m))

box :: (Monoid widget, c GTK.Box)
    => Orientation
    -> Gtk GTK.WidgetClass [BoxPack GTK.Widget] ()
    -> Gtk c widget ()
box orientation mkChildren =
  do widget <-
       liftIO (case orientation of
                 Horizontal ->
                   fmap castToBox (hBoxNew True 1)
                 Vertical ->
                   fmap castToBox (vBoxNew True 1))
     children <-
       bin (pure . BoxPack defaultPacking . GTK.toWidget) mkChildren
     rb (do initialChildren <- valueBLater children
            liftIOLater
              (do traverse_ (\(BoxPack packing a) ->
                               GTK.boxPackStart widget a packing 0)
                            initialChildren
                  GTK.boxSetHomogeneous widget False
                  widgetShow widget))
     tellCast widget

drawingArea :: (Monoid widget, c GTK.DrawingArea)
            => Gtk c widget ()
drawingArea =
  do widget <- liftIO drawingAreaNew
     rb (liftIOLater (widgetShow widget))
     tellCast widget

data LinkButton = LinkButton { _linkButtonURI :: Behavior String }

class LinkButtonURI a b | b -> a where
  linkButtonURI :: a -> b

instance (s ~ String, a ~ LinkButton) => LinkButtonURI a (Behavior s) where
  linkButtonURI = _linkButtonURI

instance (ret ~ [GTK.LinkButton -> MomentIO ()]) => LinkButtonURI (Behavior String) ret where
  linkButtonURI beh =
    [\widget ->
       do initialContents <- valueBLater beh
          liftIOLater (GTK.set widget [GTK.linkButtonURI := initialContents])]

linkButton :: (Monoid widget, c GTK.LinkButton)
           => [GTK.LinkButton -> MomentIO ()]
           -> Gtk GTK.WidgetClass (Last GTK.Widget) ()
           -> Gtk c widget ()
linkButton attrs mkChildren =
  do widget <-
       liftIO (linkButtonNew ("" :: String))
     rb (liftIOLater
           (containerForall widget
                            (containerRemove widget)))
     children <-
       bin (pure . GTK.toWidget) mkChildren
     rb (do initialChildren <- valueBLater children
            liftIOLater
              (do traverse_ (containerAdd widget) initialChildren
                  widgetShow widget))
     rb (sequence_ ((sequence attrs) widget))
     tellCast widget

-- TODO I think we need a better way to determine the grouping
data RadioButtonGroup = RadioButtonGroup (IORef (Maybe GTK.RadioButton))

newRadioButtonGroup :: MonadIO m => m RadioButtonGroup
newRadioButtonGroup = liftIO (fmap RadioButtonGroup (newIORef Nothing))

radioButton :: (Monoid widget, c GTK.RadioButton)
            => RadioButtonGroup
            -> Gtk GTK.WidgetClass (Last GTK.Widget) ()
            -> Gtk c widget ()
radioButton (RadioButtonGroup groupRef) mkChildren =
  do widget <-
       liftIO (do group <- readIORef groupRef
                  case group of
                    Just widget -> radioButtonNewFromWidget widget
                    Nothing ->
                      do widget <- radioButtonNew
                         writeIORef groupRef
                                    (Just widget)
                         return widget)
     children <-
       bin (pure . GTK.toWidget) mkChildren
     rb (do initialChildren <- valueBLater children
            liftIOLater
              (do traverse_ (containerAdd widget) initialChildren
                  widgetShow widget))
     tellCast widget

toggleButton
  :: (Monoid widget, c GTK.ToggleButton)
  => Gtk GTK.WidgetClass (Last GTK.Widget) () -> Gtk c widget ()
toggleButton mkChildren =
  do widget <- liftIO toggleButtonNew
     children <-
       rb (execWriterT
             (runReaderT (unGtk mkChildren)
                         (Cast (pure . GTK.toWidget))))
     rb (do initialChildren <- valueBLater children
            liftIOLater
              (do traverse_ (containerAdd widget) initialChildren
                  widgetShow widget))
     tellCast widget

volumeButton :: (Monoid widget, c GTK.VolumeButton)
             => Gtk c widget ()
volumeButton =
  do widget <- liftIO volumeButtonNew
     rb (liftIOLater (widgetShow widget))
     tellCast widget

levelBar :: (Monoid widget, c GTK.LevelBar)
         => Gtk c widget ()
levelBar =
  do widget <- liftIO levelBarNew
     rb (liftIOLater (widgetShow widget))
     tellCast widget

spinner :: (Monoid widget, c GTK.Spinner)
        => Gtk c widget ()
spinner =
  do widget <- liftIO spinnerNew
     rb (liftIOLater (widgetShow widget))
     tellCast widget

-- TODO This should probably take a switch function to introduce new tabs
-- TODO Maybe we could accept a Gtk NotebookPage [NotebookPage] instead, and provide a primitive to build a page. This probably natural addresses the switching problem
notebook :: (Monoid widget, c GTK.Notebook) => [(String, Gtk GTK.WidgetClass (Last GTK.Widget) ())] -> Gtk c widget ()
notebook pages =
  do widget <- liftIO notebookNew
     for_ pages
          (\(lbl,mkChildren) ->
             do children <-
                  bin (pure . GTK.toWidget) mkChildren
                rb (do initialChildren <- valueBLater children
                       liftIOLater
                         (do traverse_ (containerAdd widget) initialChildren
                             widgetShow widget)))
     tellCast widget

menuBar :: (Monoid widget, c GTK.MenuBar) => Gtk GTK.MenuItemClass [GTK.MenuItem] a -> Gtk c widget ()
menuBar mkChildren =
  do widget <- liftIO menuBarNew
     children <-
       bin (pure . GTK.toMenuItem) mkChildren
     rb (do initialChildren <- valueBLater children
            liftIOLater
              (do traverse_ (containerAdd widget) initialChildren
                  widgetShow widget))
     tellCast widget

menuItem :: (Monoid widget, c GTK.MenuItem) => Gtk GTK.WidgetClass (Last GTK.Widget) a -> Gtk GTK.MenuItemClass [GTK.MenuItem] () -> Gtk c widget ()
menuItem mkChildren mkSubMenu =
  do widget <- liftIO menuItemNew
     children <-
       bin (pure . GTK.toWidget) mkChildren
     rb (do initialChildren <- valueBLater children
            liftIOLater
              (do traverse_ (containerAdd widget) initialChildren
                  widgetShow widget))
     subMenu <-
       bin (pure . GTK.toMenuItem) mkSubMenu
     rb (do initialChildren <- valueBLater subMenu
            subWidget <- liftIO menuNew
            liftIOLater
              (case initialChildren of
                 [] -> return ()
                 items ->
                   do traverse_ (containerAdd subWidget) items
                      widgetShow widget
                      menuItemSetSubmenu widget subWidget))
     tellCast widget

frame :: (Monoid widget, c GTK.Frame) => Gtk GTK.WidgetClass (Last GTK.Widget) a -> Gtk GTK.WidgetClass (Last GTK.Widget) a -> Gtk c widget ()
frame mkLabel mkChildren =
  do widget <- liftIO frameNew
     label <- bin (pure . GTK.toWidget) mkLabel
     children <- bin (pure . GTK.toWidget) mkChildren
     rb (do initialLabel <- valueBLater label
            initialChildren <- valueBLater children
            liftIOLater
              (do traverse_ (frameSetLabelWidget widget) initialLabel
                  traverse_ (containerAdd widget) initialChildren
                  widgetShow widget))
     tellCast widget

entry :: (Monoid widget, c GTK.Entry) => Gtk c widget ()
entry =
  do widget <- liftIO entryNew
     rb (liftIOLater (widgetShow widget))
     tellCast widget

type GtkApp = Gtk GTK.WidgetClass (Last Widget) ()

app :: GtkApp
app =
  box Vertical
      (do packing GTK.PackNatural
                  (menuBar (menuItem "File"
                                     (do menuItem "New" mempty
                                         menuItem "Open" mempty)))
          paned Vertical
                (paned Horizontal sensitiveButton (scrolled (viewport drawingArea)))
                (paned Horizontal
                       (scrolled (viewport drawingArea))
                       (scrolled (viewport drawingArea)))
          checkButton "Check me"
          linkButton (linkButtonURI "http://google.com/")
                     "Google"
          box Horizontal
              (do group <- newRadioButtonGroup
                  radioButton group "A"
                  radioButton group "B")
          toggleButton "Toggle"
          volumeButton
          levelBar
          spinner
          notebook [("A","Tab A"),("B",void (button "Tab B"))]
          frame "Settings"
                (box Horizontal
                     (do "Name"
                         entry))
          return ())
  where sensitiveButton =
          mdo buttonClicked <-
                -- button (label (stepper "Hello, World" ("Ouch!" <$ buttonClicked)))
                button (label "Hello, World")
              return ()

main :: IO ()
main = runGtk app
