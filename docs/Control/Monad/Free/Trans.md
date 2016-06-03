## Module Control.Monad.Free.Trans

This module defines a stack-safe implementation of the _free monad transformer_.

#### `FreeT`

``` purescript
data FreeT f m a
```

The free monad transformer for the functor `f`.

##### Instances
``` purescript
(Functor f, Functor m) => Functor (FreeT f m)
(Functor f, Monad m) => Apply (FreeT f m)
(Functor f, Monad m) => Applicative (FreeT f m)
(Functor f, Monad m) => Bind (FreeT f m)
(Functor f, Monad m) => Monad (FreeT f m)
(Functor f) => MonadTrans (FreeT f)
(Functor f, Monad m) => MonadRec (FreeT f m)
```

#### `freeT`

``` purescript
freeT :: forall f m a. (Unit -> m (Either a (f (FreeT f m a)))) -> FreeT f m a
```

Construct a computation of type `FreeT`.

#### `resume`

``` purescript
resume :: forall f m a. (Functor f, MonadRec m) => FreeT f m a -> m (Either a (f (FreeT f m a)))
```

Unpack `FreeT`, exposing the first step of the computation.

#### `liftFreeT`

``` purescript
liftFreeT :: forall f m a. (Functor f, Monad m) => f a -> FreeT f m a
```

Lift an action from the functor `f` to a `FreeT` action.

#### `hoistFreeT`

``` purescript
hoistFreeT :: forall f m n a. (Functor f, Functor n) => (forall b. m b -> n b) -> FreeT f m a -> FreeT f n a
```

Change the underlying `Monad` for a `FreeT` action.

#### `interpret`

``` purescript
interpret :: forall f g m a. (Functor f, Functor m) => (forall b. f b -> g b) -> FreeT f m a -> FreeT g m a
```

Change the base functor `f` for a `FreeT` action.

#### `bimapFreeT`

``` purescript
bimapFreeT :: forall f g m n a. (Functor f, Functor n) => (forall b. f b -> g b) -> (forall b. m b -> n b) -> FreeT f m a -> FreeT g n a
```

Change the base functor `f` and the underlying `Monad` for a `FreeT` action.

#### `runFreeT`

``` purescript
runFreeT :: forall f m a. (Functor f, MonadRec m) => (f (FreeT f m a) -> m (FreeT f m a)) -> FreeT f m a -> m a
```

Run a `FreeT` computation to completion.


