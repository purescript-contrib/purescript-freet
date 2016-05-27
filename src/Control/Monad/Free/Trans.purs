-- | This module defines a stack-safe implementation of the _free monad transformer_.

module Control.Monad.Free.Trans
  ( FreeT()
  , freeT
  , liftFreeT
  , hoistFreeT
  , interpret
  , bimapFreeT
  , resume
  , runFreeT
  ) where

import Prelude
import Control.Bind ((<=<))
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Either (Either(Left, Right), either)
import Data.Exists (Exists, mkExists, runExists)

-- | Instead of implementing `bind` directly, we capture the bind using this data structure, to
-- | evaluate later.
data Bound f m b a = Bound (FreeT f m a) (a -> FreeT f m b)

-- | Capture a `bind` operation for the `FreeT` monad.
bound :: forall f m a b. (FreeT f m a) -> (a -> FreeT f m b) -> FreeT f m b
bound m f = Bind (mkExists (Bound m f))

-- | The free monad transformer for the functor `f`.
data FreeT f m a
  = Done a
  | LiftM (m a)
  | LiftF (f a)
  | Suspend (Unit -> FreeT f m a)
  | Bind (Exists (Bound f m a))

-- | Construct a computation of type `FreeT`.
freeT :: forall f m a. (Functor f, Monad m) => (Unit -> m (Either a (f (FreeT f m a)))) -> FreeT f m a
freeT thunk =
  Suspend (\_ ->
    lift (thunk unit) >>= (
      either
        pure
        ((_ >>= id) <<< liftFreeT)
    )
  )

-- | Unpack `FreeT`, exposing the first step of the computation.
resume :: forall f m a. (Functor f, MonadRec m) => FreeT f m a -> m (Either a (f (FreeT f m a)))
resume = tailRecM go
  where
  go :: FreeT f m a -> m (Either (FreeT f m a) (Either a (f (FreeT f m a))))
  go (Done a) = return $ Right $ Left a
  go (LiftM m) = (Right <<< Left) <$> m
  go (LiftF f) = return $ Right $ Right $ pure <$> f
  go (Suspend thunk) = return $ Left $ thunk unit
  go (Bind e) =
    runExists
      (\(Bound m f) ->
        case m of
          Done a -> return $ Left $ f a
          LiftM m2 -> (Left <<< f) <$> m2
          LiftF f2 -> return $ Right $ Right $ f <$> f2
          Suspend thunk -> return $ Left $ (thunk unit) >>= f
          Bind e1 -> runExists (\(Bound m1 f1) -> return (Left (bind m1 (\z -> f1 z >>= f)))) e1
      )
      e

instance functorFreeT :: Functor (FreeT f m) where
  map f m = m >>= (pure <<< f)

instance applyFreeT :: Apply (FreeT f m) where
  apply = ap

instance applicativeFreeT :: Applicative (FreeT f m) where
  pure = Done

instance bindFreeT :: Bind (FreeT f m) where
  bind (Done a) f = f a
  bind x@(LiftM _) f = bound x f
  bind x@(LiftF _) f = bound x f
  bind (Suspend thunk) f = bound (thunk unit) f
  bind (Bind e) f = runExists (\(Bound a k) -> bound a (\x -> bound (Suspend (\_ -> k x)) f)) e

instance monadFreeT :: Monad (FreeT f m)

instance monadTransFreeT :: MonadTrans (FreeT f) where
  lift = LiftM

instance monadRecFreeT :: (Functor f, Monad m) => MonadRec (FreeT f m) where
  tailRecM f = go
    where
    go s = do
      e <- f s
      case e of
        Left s1 -> go s1
        Right a -> return a

-- | Lift an action from the functor `f` to a `FreeT` action.
liftFreeT :: forall f m a. (Functor f, Monad m) => f a -> FreeT f m a
liftFreeT = LiftF

-- | Change the underlying `Monad` for a `FreeT` action.
hoistFreeT :: forall f m n a. (Functor f, Functor n) => (forall b. m b -> n b) -> FreeT f m a -> FreeT f n a
hoistFreeT = bimapFreeT id

-- | Change the base functor `f` for a `FreeT` action.
interpret :: forall f g m a. (Functor f, Functor m) => (forall b. f b -> g b) -> FreeT f m a -> FreeT g m a
interpret nf = bimapFreeT nf id

-- | Change the base functor `f` and the underlying `Monad` for a `FreeT` action.
bimapFreeT :: forall f g m n a. (Functor f, Functor n) => (forall b. f b -> g b) -> (forall b. m b -> n b) -> FreeT f m a -> FreeT g n a
bimapFreeT _ _ (Done a) = Done a
bimapFreeT _ nm (LiftM m) = LiftM $ nm m
bimapFreeT nf _ (LiftF f) = LiftF $ nf f
bimapFreeT nf nm (Suspend thunk) = Suspend (\_ -> bimapFreeT nf nm (thunk unit))
bimapFreeT nf nm (Bind e) = runExists (\(Bound a f) -> bound (bimapFreeT nf nm a) (bimapFreeT nf nm <<< f)) e

-- | Run a `FreeT` computation to completion.
runFreeT :: forall f m a. (Functor f, MonadRec m) => (f (FreeT f m a) -> m (FreeT f m a)) -> FreeT f m a -> m a
runFreeT interp = tailRecM (go <=< resume)
  where
  go :: Either a (f (FreeT f m a)) -> m (Either (FreeT f m a) a)
  go (Left a) = return (Right a)
  go (Right fc) = do
    c <- interp fc
    return (Left c)
