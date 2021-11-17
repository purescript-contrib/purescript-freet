module Test.CofreeTExample where

import Prelude

import Control.Comonad.Cofree.Trans (CofreeT, cofreeT, head, tail)
import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)

type IndexedList = CofreeT List Maybe Int

list :: IndexedList
list = cofreeT (go 5)
  where
  go :: Int -> Unit -> Maybe (Tuple Int (List IndexedList))
  go 0 _ = Just $ Tuple 0 Nil
  go 5 _ = Just $ Tuple 5 (fromFoldable $ cofreeT <$> (go <$> [ 4, 3 ]))
  go i _ = Just $ Tuple i (pure $ cofreeT (go (i - 1)))

annotations :: IndexedList -> List (Maybe Int)
annotations il = head il : join (annotations <$> fromMaybe mempty (tail il))

main :: Effect Unit
main = logShow $ annotations list
