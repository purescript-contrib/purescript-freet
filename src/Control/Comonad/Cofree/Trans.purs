-- | This module defines a lazy implementation of the _cofree monad transformer_.

-- | Given a `CofreeT` f w a:
-- | - 'f' is a `Functor`, generally representing an AST,
-- | - 'w' is a 'Comonad',
-- | - and 'a' is the type of the annotation.
-- |
-- | Usually, you would use `CofreeT` to annotate an existing AST with
-- | metadata such as source locations, file names, etc.

module Control.Comonad.Cofree.Trans where

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
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple as T
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

-- | The cofree comonad transformer for the functor 'f'.
newtype CofreeT f w a = CofreeT (Unit -> w (Tuple a (f (CofreeT f w a))))

-- | Construct a `CofreeT` from a lazy computation with an annotation 'a'.
cofreeT
  :: forall f w a
   . (Unit -> w (Tuple a (f (CofreeT f w a))))
  -> CofreeT f w a
cofreeT = CofreeT

-- | Construct a `CofreeT` from a computation with an annotation 'a'.
cofreeT'
  :: forall f w a
   . w (Tuple a (f (CofreeT f w a)))
  -> CofreeT f w a
cofreeT' t = CofreeT (\_ -> t)

-- | Unpack `CofreeT` into the inner computation.
runCofreeT :: forall f w a. CofreeT f w a -> w (Tuple a (f (CofreeT f w a)))
runCofreeT (CofreeT f) = f unit

-- | Obtain the annotation stored within a `CofreeT`.
head :: forall f w a. Functor w => CofreeT f w a -> w a
head = map T.fst <<< runCofreeT

-- | Obtain the inner computation stored within a `CofreeT`.
tail :: forall f w a. Functor w => CofreeT f w a -> w (f (CofreeT f w a))
tail = map T.snd <<< runCofreeT

-- Note: This cannot be automatically derived because 'a' also appears in the
-- 'fst' position of the inner Tuple.
instance functorCofreeT :: (Functor w, Functor f) => Functor (CofreeT f w) where
  map f (CofreeT inner) = CofreeT $ map (map (bimap f (map (map f)))) inner

instance applyCofreeT :: (Apply w, Apply f) => Apply (CofreeT f w) where
  apply (CofreeT innerF) (CofreeT inner) =
    CofreeT $ \_ ->
      go <$> innerF unit <*> inner unit
    where
    go (Tuple f nextF) (Tuple x nextX) =
      Tuple (f x) (lift2 (<*>) nextF nextX)

instance applicativeCofreeT :: (Applicative w, Apply f, Plus f) => Applicative (CofreeT f w) where
  pure a = CofreeT $ \_ -> pure (Tuple a empty)

instance bindCofreeT :: (Monad w, Alt f, Apply f) => Bind (CofreeT f w) where
  bind (CofreeT inner) f =
    CofreeT $ \_ -> do
      (Tuple a m) <- inner unit
      let (CofreeT next) = f a
      (Tuple b n) <- next unit
      pure $ Tuple b (n <|> map (_ >>= f) m)

instance monadCofreeT :: (Monad w, Plus f, Apply f) => Monad (CofreeT f w)

instance monadTransCofreeT :: Plus f => MonadTrans (CofreeT f) where
  lift = cofreeT' <<< map go
    where
    go x = Tuple x empty

instance monadEffectCofreeT :: (MonadEffect w, Plus f, Apply f) => MonadEffect (CofreeT f w) where
  liftEffect eff = cofreeT' $ go <$> liftEffect eff
    where
    go a = Tuple a empty

instance monadAffCofreeT :: (MonadAff w, Plus f, Apply f) => MonadAff (CofreeT f w) where
  liftAff aff = cofreeT' $ go <$> liftAff aff
    where
    go a = Tuple a empty

instance comonadCofreeCofreeT :: (Comonad w, Functor f) => ComonadCofree f (CofreeT f w) where
  unwrapCofree = extract <<< tail

instance comonadTransCofreeT :: ComonadTrans (CofreeT f) where
  lower = head

instance comonadAskCofreeT :: (Functor f, ComonadAsk e w) => ComonadAsk e (CofreeT f w) where
  ask = ask <<< tail

instance foldableCofreeT :: (Foldable w, Foldable f) => Foldable (CofreeT f w) where
  foldMap f (CofreeT inner) = foldMap go $ inner unit
    where
    go (Tuple a next) = f a <> foldMap (foldMap f) next

  foldr abb b = foldrDefault abb b

  foldl bab b = foldlDefault bab b

instance traversableCofreeT :: (Traversable w, Traversable f) => Traversable (CofreeT f w) where
  traverse f (CofreeT inner) =
    cofreeT' <$> traverse go (inner unit)
    where
    go (Tuple a next) = Tuple <$> f a <*> traverse (traverse f) next

  sequence = sequenceDefault

instance extendCofreeT :: (Comonad w, Functor f) => Extend (CofreeT f w) where
  extend f (CofreeT inner) = CofreeT $ \_ -> extend go (inner unit)
    where
    go w = Tuple (f $ cofreeT' w) $ extend f <$> T.snd (extract w)

instance comonadCofreeT :: (Comonad w, Functor f) => Comonad (CofreeT f w) where
  extract = extract <<< head

-- | 'hoist' the effect type using a natural transform.
hoistCofreeT :: forall f w u a. Functor f => Functor u => (w ~> u) -> CofreeT f w a -> CofreeT f u a
hoistCofreeT nm = bimapCofreeT identity nm

-- | 'interpret' the inner functor using a natural transform.
interpretCofreeT :: forall f g w a. Functor g => Functor w => (f ~> g) -> CofreeT f w a -> CofreeT g w a
interpretCofreeT nf = bimapCofreeT nf identity

-- | Both 'interpret' and 'hoist' the inner functor as well as the effect using natural transforms.
bimapCofreeT :: forall f g w u a. Functor u => Functor g => (f ~> g) -> (w ~> u) -> CofreeT f w a -> CofreeT g u a
bimapCofreeT nf nm (CofreeT inner) = CofreeT $ (map (map (map go))) $ map nm inner
  where
  go :: f (CofreeT f w a) -> g (CofreeT g u a)
  go = map (bimapCofreeT nf nm) <<< nf
