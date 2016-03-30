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
    done :: a -> r,
    liftM :: m a -> r,
    liftF :: f a -> r,
    suspend :: (Unit -> FreeT f m a) -> r,
    bind :: forall b. FreeT f m b -> (b -> FreeT f m a) -> r
  }
  -> r
)

done_ :: forall f m a. a -> FreeT f m a
done_ a = FreeT (\{ done: x } -> x a)

liftM_ :: forall f m a. m a -> FreeT f m a
liftM_ m = FreeT (\{ liftM: x } -> x m)

liftF_ :: forall f m a. f a -> FreeT f m a
liftF_ f = FreeT (\{ liftF: x } -> x f)

suspend_ :: forall f m a. (Unit -> FreeT f m a) -> FreeT f m a
suspend_ thunk = FreeT (\{ suspend: x } -> x thunk)

bind_ :: forall f m a b. FreeT f m a -> (a -> FreeT f m b) -> FreeT f m b
bind_ (FreeT m) f = m {
  done: (\a -> f a),
  liftM: (\m2 -> FreeT (\{ bind: x } -> x (liftM_ m2) f)),
  liftF: (\f2 -> FreeT (\{ bind: x } -> x (liftF_ f2) f)),
  suspend: (\thunk -> suspend_ (\_ -> bind (thunk unit) f)),
  bind: (\m2 f2 -> bind_ m2 (\a -> bind_ (f2 a) f))
}

instance functorFreeT :: Functor (FreeT f m) where
  map f ma = bind_ ma (done_ <<< f)

instance applyFreeT :: Apply (FreeT f m) where
  apply mf ma = bind_ mf (\f -> bind_ ma (done_ <<< f))

instance applicativeFreeT :: Applicative (FreeT f m) where
  pure a = done_ a

instance bindFreeT :: Bind (FreeT f m) where
  bind = bind_

instance monadFreeT :: Monad (FreeT f m)

instance monadTransFreeT :: MonadTrans (FreeT f) where
  lift = liftM_

instance monadRecFreeT :: MonadRec (FreeT f m) where
  tailRecM go = go2
    where
      go2 a = (go a) >>= (
        either
          go2
          pure
      )

-- | Construct a computation of type `FreeT`.
freeT :: forall f m a. (Functor f, Monad m) => (Unit -> m (Either a (f (FreeT f m a)))) -> FreeT f m a
freeT thunk =
  suspend_ (\_ ->
    (lift $ thunk unit) >>= (
      either
        pure
        ((\x -> bind_ x id) <<< liftF_)
    )
  )

resume :: forall f m a. (Functor f, MonadRec m) => FreeT f m a -> m (Either a (f (FreeT f m a)))
resume = tailRecM go
  where
    go :: FreeT f m a -> m (Either (FreeT f m a) (Either a (f (FreeT f m a))))
    go (FreeT m) = m {
      done: pure <<< Right <<< Left,
      liftM: ((Right <<< Left) <$> _),
      liftF: pure <<< Right <<< Right <<< (pure <$> _),
      suspend: (\thunk -> pure $ Left $ thunk unit),
      bind: (\(FreeT m2) f -> m2 {
        done: pure <<< Left <<< f,
        liftM: ((Left <<< f) <$> _),
        liftF: pure <<< Right <<< Right <<< (f <$> _),
        suspend: (\thunk -> pure $ Left $ bind_ (thunk unit) f),
        bind: (\m3 f2 -> pure $ Left $ (bind_ m3 (\a -> bind_ (f2 a) f)))
      })
    }

-- | Lift an action from the functor `f` to a `FreeT` action.
liftFreeT :: forall f m a. f a -> FreeT f m a
liftFreeT = liftF_

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
    go (FreeT m) = m {
      done: pure <<< Right,
      liftM: (Right <$> _),
      liftF: (Left <$> _) <<< f <<< (pure <$> _),
      suspend: (\thunk -> pure $ Left $ thunk unit),
      bind: (\(FreeT m2) f2 -> m2 {
        done: pure <<< Left <<< f2,
        liftM: ((Left <<< f2) <$> _),
        liftF: (\f3 ->
          (resume (liftF_ f3)) >>= (
            either
              (pure <<< Left <<< f2)
              ((Left <$> _) <<< f <<< ((_ >>= f2) <$> _))
          )
        ),
        suspend: (\thunk -> pure $ Left $ bind_ (thunk unit) f2),
        bind: (\m3 f3 -> pure $ Left $ (bind_ m3 (\a -> bind_ (f3 a) f2)))
      })
    }
