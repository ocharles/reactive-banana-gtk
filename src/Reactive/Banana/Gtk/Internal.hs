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
import Control.Monad.Trans.Writer.Strict (WriterT, execWriterT, mapWriterT, Writer)
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
  deriving (Functor, Applicative, Monad, MonadWriter (Behavior t widget), MonadReader (Cast c widget), MonadFix, MonadIO)

instance (a ~ (), Monoid widget) => Monoid (Gtk c widget t a) where
  mempty = return ()
  mappend = (>>)

instance Monoid a => Monoid (Behavior t a) where
  mempty = pure mempty
  mappend = liftA2 mappend

rb :: Monoid widget => Moment t a -> Gtk c widget t a
rb m = Gtk (lift (lift m))

-- TODO This doesn't allow you to 'remove' an attribute, which should set it back to its default.
newtype Attribute widget t a =
  Attribute {unAttributes :: ReaderT widget (Moment t) a}
  deriving (Functor,Monad,Applicative,MonadIO, MonadReader widget)

(<~)
  :: Frameworks t
  => Gtk.Attr widget value -> Behavior t value -> Attribute widget t ()
attr <~ value =
  do widget <- ask
     Attribute (lift (do initialValue <- initial value
                         liftIOLater
                           (Gtk.set widget [attr Gtk.:= initialValue])))

applyAttributes :: Attribute widget t a -> widget -> Moment t a
applyAttributes (Attribute m) widget =
  runReaderT m widget
