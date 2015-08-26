module Test.Main where

import Prelude

import Control.Apply
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Aff
import Control.Monad.Trans
import Control.Monad.Free.Trans
import Control.Monad.Rec.Class

import Node.ReadLine

data TeletypeF a = WriteLine String a | ReadLine (String -> a)

instance functorTeletypeF :: Functor TeletypeF where
  map f (WriteLine s a) = WriteLine s (f a)
  map f (ReadLine k) = ReadLine (f <<< k) 

type Teletype = FreeT TeletypeF

writeLine :: forall m. (Monad m) => String -> FreeT TeletypeF m Unit
writeLine s = liftFreeT (WriteLine s unit)

readLine :: forall m. (Monad m) =>  FreeT TeletypeF m String
readLine = liftFreeT (ReadLine id)

runTeletype :: forall a eff. Teletype (Aff (console :: CONSOLE | eff)) a -> Aff (console :: CONSOLE | eff) a
runTeletype = runFreeT interp
  where
  interp :: TeletypeF                      (Teletype (Aff (console :: CONSOLE | eff)) a) -> 
            Aff (console :: CONSOLE | eff) (Teletype (Aff (console :: CONSOLE | eff)) a)
  interp (WriteLine s next) = do
    liftEff (log s)
    return next
  interp (ReadLine k) = do
    s <- readLine
    return (k s)
    
  readLine :: forall eff. Aff (console :: CONSOLE | eff) String
  readLine = makeAff \_ k -> void do
    interface <- createInterface noCompletion
    setPrompt "> " 2 interface
    setLineHandler interface \s -> close interface *> k s
    line <- prompt interface
    return line

main = runAff print return $ runTeletype $ forever do
  s <- readLine
  lift $ do
    liftEff $ log "Please wait..."
    later' 1000 $ return unit
  writeLine ("You typed: " ++ s)
