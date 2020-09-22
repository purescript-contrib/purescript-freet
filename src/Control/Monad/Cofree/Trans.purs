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

import Data.Bifunctor (bimap)
import Data.Tuple (Tuple)
import Data.Tuple as T

-- | The cofree monad transformer for the functor 'f'.
newtype CofreeT f m a = CofreeT (Unit -> m (Tuple a (f (CofreeT f m a))))

-- | Construct a `CofreeT` from a lazy computation with an annotation 'a'.
cofreeT
    :: forall f m a
     . (Unit -> m (Tuple a (f (CofreeT f m a))))
    -> CofreeT f m a
cofreeT = CofreeT

-- | Unpack `CofreeT` into the inner computation.
unCofreeT :: forall f m a. CofreeT f m a -> m (Tuple a (f (CofreeT f m a)))
unCofreeT (CofreeT f) = f unit

-- | Obtain the annotation stored within a `CofreeT`.
head :: forall f m a. Functor m => CofreeT f m a -> m a
head = map T.fst <<< unCofreeT

-- | Obtain the inner computation stored within a `CofreeT`.
tail :: forall f m a. Functor m => CofreeT f m a -> m (f (CofreeT f m a))
tail = map T.snd <<< unCofreeT

-- Note: This cannot be automatically derived because 'a' also appers in the
-- 'fst' position of the inner Tuple.
instance functorCofreeT :: (Functor m, Functor f) => Functor (CofreeT f m) where
  map f (CofreeT inner) = CofreeT $ map (map (bimap f (map (map f)))) inner

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
