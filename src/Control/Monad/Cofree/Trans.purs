-- | This module defines a lazy implementation of the _cofree monad transformer_.

-- | Given a `CofreeT` f m a:
-- | - 'f' is a `Functor`, generally representing an AST,
-- | - 'm' is a 'Monad', generally representing an effect,
-- | - and 'a' is the type of the annotation.
-- |
-- | Usually, you would use `CofreeT` to annotate an existing AST with
-- | metadata such as source locations, file names, etc.

module Control.Monad.Cofree.Trans where

import Prelude

import Control.Alternative (class Alt, class Plus, empty, (<|>))
import Control.Apply (lift2)
import Control.Comonad (class Comonad, class Extend, extend, extract)
import Control.Comonad.Cofree.Class (class ComonadCofree)
import Control.Comonad.Env.Class (class ComonadAsk, ask)
import Control.Comonad.Trans.Class (class ComonadTrans)
import Control.Monad.Trans.Class (class MonadTrans)
import Data.Bifunctor (bimap)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Function (on)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple as T
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

-- | The cofree monad transformer for the functor 'f'.
newtype CofreeT f m a = CofreeT (Unit -> m (Tuple a (f (CofreeT f m a))))

-- | Construct a `CofreeT` from a lazy computation with an annotation 'a'.
cofreeT
    :: forall f m a
     . (Unit -> m (Tuple a (f (CofreeT f m a))))
    -> CofreeT f m a
cofreeT = CofreeT

-- | Construct a `CofreeT` from a computation with an annotation 'a'.
cofreeT'
    :: forall f m a
     . m (Tuple a (f (CofreeT f m a)))
    -> CofreeT f m a
cofreeT' t = CofreeT $ (\_ -> t)

-- | Unpack `CofreeT` into the inner computation.
runCofreeT :: forall f m a. CofreeT f m a -> m (Tuple a (f (CofreeT f m a)))
runCofreeT (CofreeT f) = f unit

-- | Obtain the annotation stored within a `CofreeT`.
head :: forall f m a. Functor m => CofreeT f m a -> m a
head = map T.fst <<< runCofreeT

-- | Obtain the inner computation stored within a `CofreeT`.
tail :: forall f m a. Functor m => CofreeT f m a -> m (f (CofreeT f m a))
tail = map T.snd <<< runCofreeT

-- Note: This cannot be automatically derived because 'a' also appears in the
-- 'fst' position of the inner Tuple.
instance functorCofreeT :: (Functor m, Functor f) => Functor (CofreeT f m) where
  map f (CofreeT inner) = CofreeT $ map (map (bimap f (map (map f)))) inner

instance applyCofreeT :: (Apply m, Apply f) => Apply (CofreeT f m) where
  apply (CofreeT innerF) (CofreeT inner) =
    CofreeT
      $ \_ ->
        go <$> innerF unit <*> inner unit
    where
      go (Tuple f nextF) (Tuple x nextX) =
        Tuple (f x) (lift2 (<*>) nextF nextX)

instance applicativeCofreeT :: (Applicative m, Apply f, Plus f) => Applicative (CofreeT f m) where
  pure a = CofreeT $ \_ -> pure (Tuple a empty)

instance bindCofreeT :: (Monad m, Alt f, Apply f) => Bind (CofreeT f m) where
  bind (CofreeT inner) f =
    CofreeT
      $ \_ -> do
        (Tuple a m) <- inner unit
        let (CofreeT next) = f a
        (Tuple b n) <- next unit
        pure $ Tuple b (n <|> map (_ >>= f) m)

instance monadCofreeT :: (Monad m, Plus f, Apply f) => Monad (CofreeT f m)

instance monadTransCofreeT :: Plus f => MonadTrans (CofreeT f) where
  lift = cofreeT' <<< map go
    where
      go x = Tuple x empty

instance monadEffectCofreeT :: (MonadEffect m, Plus f, Apply f) => MonadEffect (CofreeT f m) where
  liftEffect eff = cofreeT' $ go <$> liftEffect eff
    where
      go a = Tuple a empty

instance monadAffCofreeT :: (MonadAff m, Plus f, Apply f) => MonadAff (CofreeT f m) where
  liftAff aff = cofreeT' $ go <$> liftAff aff
    where
      go a = Tuple a empty

instance comonadCofreeCofreeT :: (Comonad m, Functor f) => ComonadCofree f (CofreeT f m) where
  unwrapCofree = extract <<< tail

instance comonadTransCofreeT :: ComonadTrans (CofreeT f) where
  lower = head

instance comonadAskCofreeT :: (Functor f, ComonadAsk e m) => ComonadAsk e (CofreeT f m) where
  ask = ask <<< tail

instance foldableCofreeT :: (Foldable m, Foldable f) => Foldable (CofreeT f m) where
  foldMap f (CofreeT inner) = foldMap go $ inner unit
    where
      go (Tuple a next) = f a <> foldMap (foldMap f) next

  foldr abb b = foldrDefault abb b

  foldl bab b = foldlDefault bab b

instance traversableCofreeT :: (Traversable m, Traversable f) => Traversable (CofreeT f m) where
  traverse f (CofreeT inner) =
    cofreeT' <$> traverse go (inner unit)
    where
      go (Tuple a next) = Tuple <$> f a <*> traverse (traverse f) next

  sequence = sequenceDefault

instance extendCofreeT :: (Comonad m, Functor f) => Extend (CofreeT f m) where
  extend f (CofreeT inner) = CofreeT $ \_ -> extend go (inner unit)
    where
      go w = Tuple (f $ cofreeT' w) $ extend f <$> T.snd (extract w)

instance comonadCofreeT :: (Comonad m, Functor f) => Comonad (CofreeT f m) where
  extract = extract <<< head

instance eqCofreeT :: Eq (m (Tuple a (f (CofreeT f m a)))) => Eq (CofreeT f m a) where
  eq = eq `on` runCofreeT

instance ordCofreeT :: Ord (m (Tuple a (f (CofreeT f m a)))) => Ord (CofreeT f m a) where
  compare = compare `on` runCofreeT

-- | 'hoist' the effect type using a natural transform.
hoistCofreeT :: forall f m n a. Functor f => Functor n => (m ~> n) -> CofreeT f m a -> CofreeT f n a
hoistCofreeT nm = bimapCofreeT identity nm

-- | 'interpret' the inner functor using a natural transform.
interpretCofreeT :: forall f g m a. Functor g => Functor m => (f ~> g) -> CofreeT f m a -> CofreeT g m a
interpretCofreeT nf = bimapCofreeT nf identity

-- | Both 'interpret' and 'hoist' the inner functor as well as the effect using natural transforms.
bimapCofreeT :: forall f g m n a. Functor n => Functor g => (f ~> g) -> (m ~> n) -> CofreeT f m a -> CofreeT g n a
bimapCofreeT nf nm (CofreeT inner) = CofreeT $ (map (map (map go))) $ map nm inner
  where
    go :: f (CofreeT f m a) -> g (CofreeT g n a)
    go = map (bimapCofreeT nf nm) <<< nf
