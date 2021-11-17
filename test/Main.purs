module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Control.Monad.Free.Trans (FreeT, runFreeT, liftFreeT)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.Trans.Class (lift)

data TeletypeF a
  = WriteLine String a
  | ReadLine (String -> a)

instance functorTeletypeF :: Functor TeletypeF where
  map f (WriteLine s a) = WriteLine s (f a)
  map f (ReadLine k) = ReadLine (f <<< k)

type Teletype = FreeT TeletypeF

writeLine :: forall m. Monad m => String -> FreeT TeletypeF m Unit
writeLine s = liftFreeT (WriteLine s unit)

readLine :: forall m. Monad m => FreeT TeletypeF m String
readLine = liftFreeT (ReadLine identity)

mockTeletype :: forall a. Teletype Effect a -> Effect a
mockTeletype = runFreeT interp
  where
  interp (WriteLine s next) = do
    liftEffect (log s)
    pure next
  interp (ReadLine k) = do
    pure (k "Fake input")

-- Also see purescript-safely
replicateM_ :: forall m a. MonadRec m => Int -> m a -> m Unit
replicateM_ n x = tailRecM step n
  where
  step :: Int -> m (Step Int Unit)
  step 0 = pure (Done unit)
  step m = x $> Loop (m - 1)

main :: Effect Unit
main = mockTeletype $ replicateM_ 15000 do
  lift $ log "Enter some input:"
  s <- readLine
  writeLine ("You typed: " <> s)
