module Control.Monad.Cofree.Trans where

import Prelude

import Data.Bifunctor (bimap)
import Data.Tuple (Tuple)
import Data.Tuple as T

newtype CofreeT f m a = CofreeT (Unit -> m (Tuple a (f (CofreeT f m a))))

cofreeT
    :: forall f m a
     . (Unit -> m (Tuple a (f (CofreeT f m a))))
    -> CofreeT f m a
cofreeT = CofreeT

unCofreeT :: forall f m a. CofreeT f m a -> m (Tuple a (f (CofreeT f m a)))
unCofreeT (CofreeT f) = f unit

head :: forall f m a. Functor m => CofreeT f m a -> m a
head = map T.fst <<< unCofreeT

tail :: forall f m a. Functor m => CofreeT f m a -> m (f (CofreeT f m a))
tail = map T.snd <<< unCofreeT

instance functorCofreeT :: (Functor m, Functor f) => Functor (CofreeT f m) where
  map f (CofreeT inner) = CofreeT $ map (map (bimap f (map (map f)))) inner

hoistCofreeT :: forall f m n a. Functor f => Functor n => (m ~> n) -> CofreeT f m a -> CofreeT f n a
hoistCofreeT nm = bimapCofreeT identity nm

interpretCofreeT :: forall f g m a. Functor g => Functor m => (f ~> g) -> CofreeT f m a -> CofreeT g m a
interpretCofreeT nf = bimapCofreeT nf identity

bimapCofreeT :: forall f g m n a. Functor n => Functor g => (f ~> g) -> (m ~> n) -> CofreeT f m a -> CofreeT g n a
bimapCofreeT nf nm (CofreeT inner) = CofreeT $ (map (map (map go))) $ map nm inner
  where
    go :: f (CofreeT f m a) -> g (CofreeT g n a)
    go = map (bimapCofreeT nf nm) <<< nf
