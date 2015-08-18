module Reactive.Banana.Gtk.Internal where

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
import Reactive.Banana (Behavior, Moment, Event, stepper)
import Data.Foldable (traverse_, for_)
import Reactive.Banana.Frameworks
       (Frameworks, compile, actuate, reactimate', changes, initial,
        liftIOLater, newEvent)
import qualified Graphics.UI.Gtk as Gtk

data Cast c b where
  Cast :: (forall a. c a => (a -> b)) -> Cast c b

tellCast x =
  do Cast cast <- ask
     tell (pure (cast x))

bin :: (Monoid widget2) => (forall a. c1 a => a -> widget1) -> Gtk c1 widget1 t x -> Gtk c2 widget2 t (Behavior t widget1)
bin cast mkChildren =
  rb (execWriterT
        (runReaderT (unGtk mkChildren)
                    (Cast cast)))

newtype Gtk c widget t a =
  Gtk {unGtk :: ReaderT (Cast c widget) (WriterT (Behavior t widget) (Moment t)) a}

instance (a ~ (), Monoid widget) => Monoid (Gtk c widget t a) where
  mempty = return ()
  mappend = (>>)

instance Functor (Gtk c widget t) where
  fmap f (Gtk m) = Gtk (fmap f m)

instance Monoid widget => Applicative (Gtk c widget t) where
  pure a = Gtk (pure a)
  Gtk f <*> Gtk a = Gtk (f <*> a)

instance Monoid widget => Monad (Gtk c widget t) where
  return a = Gtk (pure a)
  Gtk a >>= f = Gtk (a >>= unGtk . f)

instance Monoid widget => MonadWriter (Behavior t widget) (Gtk c widget t) where
  tell x = Gtk (tell x)

instance Monoid widget => MonadReader (Cast c widget) (Gtk c widget t) where
  ask = Gtk ask

instance Monoid widget => MonadFix (Gtk c widget t) where
  mfix f = Gtk (mfix (unGtk . f))

instance (Monoid widget,Frameworks t) => MonadIO (Gtk c widget t) where
  liftIO io = Gtk (liftIO io)

instance Monoid a => Monoid (Behavior t a) where
  mempty = pure mempty
  mappend = liftA2 mappend

rb :: Monoid widget => Moment t a -> Gtk c widget t a
rb m = Gtk (lift (lift m))
