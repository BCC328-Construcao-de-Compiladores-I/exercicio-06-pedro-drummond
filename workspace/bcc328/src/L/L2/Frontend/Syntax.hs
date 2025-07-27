module L.L2.Frontend.Syntax where

import Utils.Pretty 
import Utils.Value
import Utils.Var

data L2
  = L2 [S2]
    deriving (Eq, Ord, Show)


data S2
  = Def Var E2 [S2]
  | LRead String Var
  | LPrint E2
  | LAssign Var E2
  deriving (Eq, Ord, Show)

data E2
  = LVal Value
  | LVar Var
  | LAdd E2 E2
  | LMinus E2 E2
  | LMul E2 E2
  | LDiv E2 E2
  deriving (Eq, Ord, Show)

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

instance Pretty L2 where
  ppr (L2 ss) = vcat (map ppr ss)

instance Pretty S2 where
  ppr (LRead s v)
    = hsep [ text "read("
           , doubleQuotes (text s)
           , comma
           , ppr v
           , text ");"
           ]
  ppr (LPrint e)
    = hsep [ text "print("
           , ppr e
           , text ");"
           ]
  ppr (LAssign v e)
    = hsep [ ppr v
           , text ":="
           , ppr e
           , semi
           ]
  ppr (Def v e stmts)
    = hang (hsep [text "def", ppr v, text ":=", ppr e, text "in"])
        2 (vcat (map ppr stmts)) $$
      text "end"

instance Pretty E2 where
  ppr = pprPrec 0
    where
      -- precedencia 0: +, -
      pprPrec n (LAdd e1 e2)   = parensIf (n > 0) $ pprPrec 0 e1 <+> text "+" <+> pprPrec 1 e2
      pprPrec n (LMinus e1 e2) = parensIf (n > 0) $ pprPrec 0 e1 <+> text "-" <+> pprPrec 1 e2
      -- precedencia : *, /
      pprPrec n (LMul e1 e2)   = parensIf (n > 1) $ pprPrec 1 e1 <+> text "*" <+> pprPrec 2 e2
      pprPrec n (LDiv e1 e2)   = parensIf (n > 1) $ pprPrec 1 e1 <+> text "/" <+> pprPrec 2 e2
      -- precedencia var e val
      pprPrec _ (LVal v)       = ppr v
      pprPrec _ (LVar v)       = ppr v