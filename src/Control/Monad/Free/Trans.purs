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

import Data.Either (Either(..), either)

import Control.Monad.Rec.Class (MonadRec, tailRecM)
import Control.Monad.Trans (MonadTrans)

data StacklessF f m a
  = F (f (FreeT f m a))
  | Suspend (Unit -> SuspT f m a)
  | Done a

newtype SuspT f m a = SuspT (m (StacklessF f m a))

unSuspT :: forall f m a. SuspT f m a -> m (StacklessF f m a)
unSuspT (SuspT a) = a

suspend :: forall f m a. (Applicative m) => (Unit -> SuspT f m a) -> SuspT f m a
suspend thunk = SuspT $ return $ Suspend thunk

done :: forall f m a. (Applicative m) => a -> SuspT f m a
done a = SuspT $ Done <$> pure a

suspTBind :: forall f m a b. (Functor f, Monad m) => SuspT f m a -> (a -> SuspT f m b) -> SuspT f m b
suspTBind (SuspT m) f = SuspT $ do
  x <- m
  unSuspT $ go x
  where
    go :: StacklessF f m a -> SuspT f m b
    go (F fa) = SuspT $ pure $ F ((\(FreeT c) -> FreeT (\k -> (c (\a -> suspTBind (f a) k)))) <$> fa)
    go (Suspend thunk) = let x = thunk unit in SuspT $ pure $ Suspend (\_ -> suspTBind x f)
    go (Done a) = f a

bimapSuspT :: forall f g m n a. (Functor f, Functor g, Monad m, Monad n) => (forall b. f b -> g b) -> (forall b. m b -> n b) -> SuspT f m a -> SuspT g n a
bimapSuspT fg mn (SuspT m) =
  SuspT $ go <$> (mn m)
  where
    go :: StacklessF f m a -> StacklessF g n a
    go (F fa) = F (fg $ (bimapFreeT fg mn) <$> fa)
    go (Suspend thunk) = let x2 = thunk unit in Suspend (\_ -> bimapSuspT fg mn x2)
    go (Done a) = Done a

-- | The free monad transformer for the functor `f`.
newtype FreeT f m a = FreeT (forall r. (a -> SuspT f m r) -> SuspT f m r)

unFreeT :: forall f m a. FreeT f m a -> (forall r. (a -> SuspT f m r) -> SuspT f m r)
unFreeT (FreeT a) = a

-- | Construct a computation of type `FreeT`.
freeT :: forall f m a. (Functor f, Monad m) => (Unit -> m (Either a (f (FreeT f m a)))) -> FreeT f m a
freeT thunk = FreeT (\k -> SuspT $ do
    x <- thunk unit
    unSuspT $ either
      k
      (\fa -> suspTBind (SuspT $ pure $ F fa) k)
      x
  )

resume :: forall f m a. (Functor f, MonadRec m) => FreeT f m a -> m (Either a (f (FreeT f m a)))
resume (FreeT c) = (unSuspT $ c done) >>= (tailRecM go)
  where
    go :: StacklessF f m a -> m (Either (StacklessF f m a) (Either a (f (FreeT f m a))))
    go (F fa) = return $ Right $ Right fa
    go (Suspend thunk) = Left <$> (unSuspT $ thunk unit)
    go (Done a) = return $ Right $ Left a

instance functorFreeT :: (Applicative m) => Functor (FreeT f m) where
  map f (FreeT ca) = FreeT (\k -> suspend (\_ -> ca (k <<< f)))

instance applyFreeT :: (Applicative m) => Apply (FreeT f m) where
  apply (FreeT cf) (FreeT ca) = FreeT (\k -> suspend (\_ -> cf (\f -> suspend (\_ -> ca (k <<< f)))))

instance applicativeFreeT :: (Applicative m) => Applicative (FreeT f m) where
  pure a = FreeT (\k -> suspend (\_ -> k a))

instance bindFreeT :: (Applicative m) => Bind (FreeT f m) where
  bind (FreeT ca) f = FreeT (\k -> suspend (\_ -> ca (\a -> (unFreeT $ f a) k)))

instance monadFreeT :: (Applicative m) => Monad (FreeT f m)

instance monadTransFreeT :: MonadTrans (FreeT f) where
  lift m = FreeT (\k -> SuspT $ do
    a <- m
    unSuspT $ k a
  )

instance monadRecFreeT :: (Applicative m) => MonadRec (FreeT f m) where
  tailRecM f = go
    where
    go s = do
      e <- f s
      case e of
        Left s1 -> go s1
        Right a -> return a

-- | Lift an action from the functor `f` to a `FreeT` action.
liftFreeT :: forall f m a. (Functor f, Monad m) => f a -> FreeT f m a
liftFreeT fa = FreeT (suspTBind (SuspT $ pure $ F (pure <$> fa)))

-- | Change the underlying `Monad` for a `FreeT` action.
hoistFreeT :: forall f m n a. (Functor f, Monad m, Monad n) => (forall b. m b -> n b) -> FreeT f m a -> FreeT f n a
hoistFreeT = bimapFreeT id

-- | Change the base functor `f` for a `FreeT` action.
interpret :: forall f g m a. (Functor f, Functor g, Monad m) => (forall b. f b -> g b) -> FreeT f m a -> FreeT g m a
interpret nf = bimapFreeT nf id

-- | Change the base functor `f` and the underlying `Monad` for a `FreeT` action.
bimapFreeT :: forall f g m n a. (Functor f, Functor g, Monad m, Monad n) => (forall b. f b -> g b) -> (forall b. m b -> n b) -> FreeT f m a -> FreeT g n a
bimapFreeT fg mn (FreeT c) = FreeT (\k -> suspTBind (bimapSuspT fg mn (c done)) k)

-- | Run a `FreeT` computation to completion.
runFreeT :: forall f m a. (MonadRec m) => (f (FreeT f m a) -> m (FreeT f m a)) -> FreeT f m a -> m a
runFreeT f (FreeT c) = (unSuspT $ c done) >>= tailRecM go
  where
    go :: StacklessF f m a -> m (Either (StacklessF f m a) a)
    go (F fa) = (f fa) >>= (\(FreeT c2) -> Left <$> ((unSuspT $ c2 done) :: m (StacklessF f m a)))
    go (Suspend thunk) = Left <$> unSuspT (thunk unit)
    go (Done a) = return $ Right a
