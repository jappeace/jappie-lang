-- | asserts alpha equivelance by renaming shadowed bindings
--   https://en.wikipedia.org/wiki/Lambda_calculus#Alpha_equivalence
module JappieLang.Rename
  ( rename
  )
where

import JappieLang.SyntaxTree.Name
import JappieLang.SyntaxTree.Core
import qualified Data.MultiSet as MultiSet
import qualified Data.Map.Lazy as Map
import Data.MultiSet(MultiSet)
import Data.Map.Lazy(Map)
import Data.Maybe

rename :: CoreExpression -> CoreExpression
rename = renameOccurences mempty mempty

renameOccurences :: MultiSet Name -> Map Name Name -> CoreExpression -> CoreExpression
renameOccurences previousLams renames = \case
  Var name -> Var $ fromMaybe name $ Map.lookup name renames
  App leftExpr rightExpr -> App
    (renameOccurences previousLams renames leftExpr)
    (renameOccurences previousLams renames rightExpr)
  Lam name expr -> let
    occurences :: Int
    occurences = MultiSet.occur name previousLams
    newSet = MultiSet.insert name previousLams
    newName = name { shadowBust = fromIntegral occurences }
    newRenamesMap = Map.insert name newName renames
    in
    Lam newName (renameOccurences newSet newRenamesMap expr)
