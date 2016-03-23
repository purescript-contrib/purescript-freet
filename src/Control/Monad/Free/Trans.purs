-- | This module defines a stack-safe implementation of the _free monad transformer_.

module Control.Monad.Free.Trans
  ( FreeT()
  , freeT
  , liftFreeT
  {-
  , hoistFreeT
  , interpret
  , bimapFreeT
  -}
  , resume
  , runFreeT
  ) where

import Prelude

import Data.Either (Either(..), either)

import Control.Monad.Rec.Class (MonadRec, tailRecM)
import Control.Monad.Trans (MonadTrans, lift)

-- | The free monad transformer for the functor `f`.
newtype FreeT f m a = FreeT (
  forall r. {
    pureFreeT :: a -> r,
    bindFreeT :: forall b. FreeT f m b -> (b -> FreeT f m a) -> r,
    liftMFreeT :: m a -> r,
    liftFFreeT :: f (FreeT f m a) -> r,
    suspendFreeT :: (Unit -> FreeT f m a) -> r
  }
  -> r
)

mapFreeT_ :: forall f m a b. (a -> b) -> FreeT f m a -> FreeT f m b
mapFreeT_ f m = m >>= (pure <<< f)

applyFreeT_ :: forall f m a b. FreeT f m (a -> b) -> FreeT f m a -> FreeT f m b
applyFreeT_ mf ma = do
  f <- mf
  f <$> ma

suspend :: forall f m a. (Unit -> FreeT f m a) -> FreeT f m a
suspend thunk = FreeT (\{ suspendFreeT } -> suspendFreeT thunk)

liftF :: forall f m a. f (FreeT f m a) -> FreeT f m a
liftF f = FreeT (\{ liftFFreeT } -> liftFFreeT f)

instance functorFreeT :: Functor (FreeT f m) where
  map = mapFreeT_

instance applyFreeT :: Apply (FreeT f m) where
  apply mf ma = applyFreeT_ mf ma

instance applicativeFreeT :: Applicative (FreeT f m) where
  pure a = FreeT (\{ pureFreeT } -> pureFreeT a)

instance bindFreeT :: Bind (FreeT f m) where
  bind m f = FreeT (\{ bindFreeT } -> bindFreeT m f)

instance monadFreeT :: Monad (FreeT f m)

instance monadTransFreeT :: MonadTrans (FreeT f) where
  lift m = FreeT (\{ liftMFreeT } -> liftMFreeT m)

instance monadRecFreeT :: MonadRec (FreeT f m) where
  tailRecM go a = suspend (\_ ->
    (go a) >>= (
      either
        (tailRecM go)
        pure
    )
  )

-- | Construct a computation of type `FreeT`.
freeT :: forall f m a. (Functor f, Monad m) => (Unit -> m (Either a (f (FreeT f m a)))) -> FreeT f m a
freeT thunk =
  (lift (thunk unit)) >>= (
    either
      pure
      liftF
  )

resumeStep :: forall f m a. (Functor f, Monad m) => {
  pureFreeT :: a -> m (Either (FreeT f m a) (Either a (f (FreeT f m a)))),
  bindFreeT :: forall b. FreeT f m b -> (b -> FreeT f m a) -> m (Either (FreeT f m a) (Either a (f (FreeT f m a)))),
  liftMFreeT :: m a -> m (Either (FreeT f m a) (Either a (f (FreeT f m a)))),
  liftFFreeT :: f (FreeT f m a) -> m (Either (FreeT f m a) (Either a (f (FreeT f m a)))),
  suspendFreeT :: (Unit -> FreeT f m a) -> m (Either (FreeT f m a) (Either a (f (FreeT f m a))))
}
resumeStep = {
  pureFreeT: (\a -> return $ Right $ Left a),
  bindFreeT: (\(FreeT m1) f1 ->
    m1 {
      pureFreeT: (\a -> return $ Left $ f1 a),
      bindFreeT: (\m2 f2 ->
        return $ Left $ m2 >>= (\a -> (f2 a) >>= f1)
      ),
      liftMFreeT: (\m2 -> do
        a <- m2
        return $ Left $ f1 a
      ),
      liftFFreeT: (\f -> return $ Left $ liftF $ (\x -> x >>= f1) <$> f),
      suspendFreeT: (\thunk -> return $ Left $ (thunk unit) >>= f1)
    }
  ),
  liftMFreeT: (\m -> (Right <<< Left) <$> m),
  liftFFreeT: (\f -> (pure <<< Right <<< Right) f),
  suspendFreeT: (\thunk -> return $ Left $ thunk unit)
}

resume :: forall f m a. (Functor f, MonadRec m) => FreeT f m a -> m (Either a (f (FreeT f m a)))
resume = tailRecM go
  where
    go :: FreeT f m a -> m (Either (FreeT f m a) (Either a (f (FreeT f m a))))
    go (FreeT free) = free resumeStep

-- | Lift an action from the functor `f` to a `FreeT` action.
liftFreeT :: forall f m a. (Functor f, Monad m) => f a -> FreeT f m a
liftFreeT fa = FreeT (\{ liftFFreeT } -> liftFFreeT $ pure <$> fa)

{-
-- | Change the underlying `Monad` for a `FreeT` action.
hoistFreeT :: forall f m n a. (Functor f, Monad m, Monad n) => (forall b. m b -> n b) -> FreeT f m a -> FreeT f n a
hoistFreeT = bimapFreeT id

-- | Change the base functor `f` for a `FreeT` action.
interpret :: forall f g m a. (Functor f, Functor g, Monad m) => (forall b. f b -> g b) -> FreeT f m a -> FreeT g m a
interpret nf = bimapFreeT nf id

-- | Change the base functor `f` and the underlying `Monad` for a `FreeT` action.
bimapFreeT :: forall f g m n a. (Functor f, Functor g, Monad m, Monad n) => (forall b. f b -> g b) -> (forall b. m b -> n b) -> FreeT f m a -> FreeT g n a
bimapFreeT fg mn (FreeT c) = FreeT (\k -> suspTBind (bimapSuspT fg mn (c done)) k)
-}

-- | Run a `FreeT` computation to completion.
runFreeT :: forall f m a. (Functor f, MonadRec m) => (f (FreeT f m a) -> m (FreeT f m a)) -> FreeT f m a -> m a
runFreeT f = tailRecM go
  where
    go :: FreeT f m a -> m (Either (FreeT f m a) a)
    go free =
      (resume free) >>= (
        either
          (pure <<< Right)
          ((Left <$> _) <<< f)
      )
