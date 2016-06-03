module Test.Main where

import Prelude

import Control.Apply
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Trans
import Control.Monad.Free.Trans
import Control.Monad.Rec.Class

data TeletypeF a
  = WriteLine String a
  | ReadLine (String -> a)

instance functorTeletypeF :: Functor TeletypeF where
  map f (WriteLine s a) = WriteLine s (f a)
  map f (ReadLine k) = ReadLine (f <<< k)

type Teletype = FreeT TeletypeF

writeLine :: forall m. Monad m => String -> FreeT TeletypeF m Unit
writeLine s = liftFreeT (WriteLine s unit)

readLine :: forall m. Monad m =>  FreeT TeletypeF m String
readLine = liftFreeT (ReadLine id)

mockTeletype :: forall a eff. Teletype (Eff (console :: CONSOLE | eff)) a -> Eff (console :: CONSOLE | eff) a
mockTeletype = runFreeT interp
  where
    interp (WriteLine s next) = do
      liftEff (log s)
      pure next
    interp (ReadLine k) = do
      pure (k "Fake input")

main = mockTeletype $ forever do
  lift $ log "Enter some input:"
  s <- readLine
  writeLine ("You typed: " <> s)
