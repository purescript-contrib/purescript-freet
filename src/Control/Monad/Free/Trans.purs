-- | This module defines a stack-safe implementation of the _free monad transformer_.

module Control.Monad.Free.Trans
  ( FreeT
  , freeT
  , liftFreeT
  , hoistFreeT
  , interpret
  , bimapFreeT
  , resume
  , runFreeT
  ) where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Exists (Exists, mkExists, runExists)
import Data.Monoid (class Monoid, mempty)

import Control.Apply (lift2)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.Trans.Class (class MonadTrans)

-- | Instead of implementing `bind` directly, we capture the bind using this data structure, to
-- | evaluate later.
data Bound f m b a = Bound (Unit -> FreeT f m a) (a -> FreeT f m b)

-- | Capture a `bind` operation for the `FreeT` monad.
bound :: forall f m a b. (Unit -> FreeT f m a) -> (a -> FreeT f m b) -> FreeT f m b
bound m f = Bind (mkExists (Bound m f))

-- | The free monad transformer for the functor `f`.
data FreeT f m a = FreeT (Unit -> m (Either a (f (FreeT f m a)))) | Bind (Exists (Bound f m a))

-- | Construct a computation of type `FreeT`.
freeT :: forall f m a. (Unit -> m (Either a (f (FreeT f m a)))) -> FreeT f m a
freeT = FreeT

-- | Unpack `FreeT`, exposing the first step of the computation.
resume :: forall f m a. (Functor f, MonadRec m) => FreeT f m a -> m (Either a (f (FreeT f m a)))
resume = tailRecM go
  where
  go :: FreeT f m a -> m (Step (FreeT f m a) (Either a (f (FreeT f m a))))
  go (FreeT f) = map Done (f unit)
  go (Bind e) = runExists (\(Bound bound f) ->
    case bound unit of
      FreeT m ->
        m unit >>= case _ of
          Left a -> pure (Loop (f a))
          Right fc -> pure (Done (Right (map (\h -> h >>= f) fc)))
      Bind e1 -> runExists (\(Bound m1 f1) -> pure (Loop (bind (m1 unit) (\z -> f1 z >>= f)))) e1) e

instance functorFreeT :: (Functor f, Functor m) => Functor (FreeT f m) where
  map f (FreeT m) = FreeT \_ -> map (bimap f (map (map f))) (m unit)
  map f (Bind e) = runExists (\(Bound a k) -> bound a (map f <<< k)) e

instance applyFreeT :: (Functor f, Monad m) => Apply (FreeT f m) where
  apply = ap

instance applicativeFreeT :: (Functor f, Monad m) => Applicative (FreeT f m) where
  pure a = FreeT \_ -> pure (Left a)

instance bindFreeT :: (Functor f, Monad m) => Bind (FreeT f m) where
  bind (Bind e) f = runExists (\(Bound a k) -> bound a (\x -> bound (\_ -> k x) f)) e
  bind a f = bound (\_ -> a) f

instance monadFreeT :: (Functor f, Monad m) => Monad (FreeT f m)

instance monadTransFreeT :: (Functor f) => MonadTrans (FreeT f) where
  lift ma = FreeT \_ -> map Left ma

instance monadRecFreeT :: (Functor f, Monad m) => MonadRec (FreeT f m) where
  tailRecM f = go
    where
    go s =
      f s >>= case _ of
        Loop s1 -> go s1
        Done a -> pure a

instance semigroupFreeT :: (Functor f, Monad m, Semigroup w) => Semigroup (FreeT f m w) where
  append = lift2 append

instance monoidFreeT :: (Functor f, Monad m, Monoid w) => Monoid (FreeT f m w) where
  mempty = pure mempty

-- | Lift an action from the functor `f` to a `FreeT` action.
liftFreeT :: forall f m a. (Functor f, Monad m) => f a -> FreeT f m a
liftFreeT fa = FreeT \_ -> pure (Right (map pure fa))

-- | Change the underlying `Monad` for a `FreeT` action.
hoistFreeT :: forall f m n a. (Functor f, Functor n) => (m ~> n) -> FreeT f m a -> FreeT f n a
hoistFreeT = bimapFreeT id

-- | Change the base functor `f` for a `FreeT` action.
interpret :: forall f g m a. (Functor f, Functor m) => (f ~> g) -> FreeT f m a -> FreeT g m a
interpret nf = bimapFreeT nf id

-- | Change the base functor `f` and the underlying `Monad` for a `FreeT` action.
bimapFreeT :: forall f g m n a. (Functor f, Functor n) => (f ~> g) -> (m ~> n) -> FreeT f m a -> FreeT g n a
bimapFreeT nf nm (Bind e) = runExists (\(Bound a f) -> bound (bimapFreeT nf nm <<< a) (bimapFreeT nf nm <<< f)) e
bimapFreeT nf nm (FreeT m) = FreeT \_ -> map (nf <<< map (bimapFreeT nf nm)) <$> nm (m unit)

-- | Run a `FreeT` computation to completion.
runFreeT :: forall f m a. (Functor f, MonadRec m) => (f (FreeT f m a) -> m (FreeT f m a)) -> FreeT f m a -> m a
runFreeT interp = tailRecM (go <=< resume)
  where
  go :: Either a (f (FreeT f m a)) -> m (Step (FreeT f m a) a)
  go (Left a) = pure (Done a)
  go (Right fc) = Loop <$> interp fc
