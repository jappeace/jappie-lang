module JappieLang.SyntaxTree.Name
  ( Name(..)
  , prettyName
  , mkName
  )
where

import Data.Text(Text, pack)
import Data.String
import Prettyprinter
import Prettyprinter.Render.Terminal

data Name = MkName { humanName :: Text,
                     shadowBust :: Word -- | some internal counter that get's increased everytime this name get's shadowed
                   }
  deriving stock (Eq, Show)

instance IsString Name where
  fromString str = MkName {humanName = pack str, shadowBust = 0}

mkName :: Text -> Name
mkName txt = MkName { humanName = txt, shadowBust = 0 }

prettyName :: Name -> Doc AnsiStyle
prettyName (MkName name _) = pretty name
