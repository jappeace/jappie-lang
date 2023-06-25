{-# LANGUAGE RecordWildCards #-}
-- | Takes an AST
module JappieLang.CodeGen
  ( runFilePrint
  , runTextPrint
  , runFileExpr
  , runTextExpr
  )
where

import Data.Bifunctor
import JappieLang.Parser
import Text.Trifecta.Delta
import JappieLang.Print
import JappieLang.SyntaxTree.Parsed
import JappieLang.SyntaxTree.Core
import JappieLang.SyntaxTree.Name
import Data.Text.Lazy
import qualified Data.Text as SText
import Prettyprinter
import Prettyprinter.Render.Terminal
import Text.Trifecta.Result
import JappieLang.Simplify
import JappieLang.Eval
import JappieLang.Rename

-- https://hackage.haskell.org/package/x86-64bit-0.4.6.3/docs/CodeGen-X86-Asm.html#t:CodeLine
tox86Asm :: CoreExpression -> [CodeLine]
tox86Asm =  tox86AsmNames mempty

tox86AsmNames :: Set Name -> CoreExpression -> [CodeLine]
tox86AsmNames namesSeen = \case
  Var name -> [] -- TODO jmp to var?
  App left right -> to86AsmApply left right
  Lam name expr ->
    let
      label = Label (length namesSeen)
      newSeen = Set.insert name namesSeen
    in
    if
    Set.member name nameSeen
    then error "dup name found" <> show name " in " <> show state
    else
      : [ ]

to86AsmApply :: CoreExpression -> CoreExpression -> [CodeLine]
to86AsmApply appliedTo appliedWith = case appliedTo of
  Var name -> -- I guess this'd be afunction call to name? or in eval this is an error condition
  App expr1 expr2 ->
    -- in eval we first apply these two together,
    -- then we use the result to apply further...
  Lam name expr -> -- substitute all name in expr
