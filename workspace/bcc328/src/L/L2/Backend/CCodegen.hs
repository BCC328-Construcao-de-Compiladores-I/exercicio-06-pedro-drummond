module L.L2.Backend.CCodegen where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad (unless)
import Data.Set (Set)
import qualified Data.Set as Set

import L.L2.Frontend.Syntax
import Utils.Value
import Utils.Var

newtype CgEnv = CgEnv { declared :: Set Var }

type CgM a = WriterT String (State CgEnv) a

cCodegen :: L2 -> String
cCodegen ast = unlines
  [ "#include <stdio.h>"
  , "#include <stdlib.h>"
  , ""
  , "int main() {"
  , "    " ++ evalState (execWriterT (codeGenL2 ast)) (CgEnv Set.empty)
  , "    return 0;"
  , "}"
  ]

-- Helper para converter um Var para um nome de variável C válido.
varName :: Var -> String
varName (Var s) = "" ++ s

-- Gera código para L2.
codeGenL2 :: L2 -> CgM ()
codeGenL2 (L2 stmts) = mapM_ codeGenS stmts

-- Gera uma instrução C a partir de S2.
codeGenS :: S2 -> CgM ()
codeGenS (LPrint expr) = do
  cExpr <- codeGenE expr
  tell $ "    printf(\"%d\\n\", " ++ cExpr ++ ");\n"

codeGenS (LAssign var expr) = do
  cExpr <- codeGenE expr
  env <- get
  unless (var `Set.member` declared env) $ do
    tell $ "    int " ++ varName var ++ " = 0;\n"
    put (env { declared = Set.insert var (declared env) })
  tell $ "    " ++ varName var ++ " = " ++ cExpr ++ ";\n"

codeGenS (LRead prompt var) = do
  env <- get
  unless (var `Set.member` declared env) $ do
    tell $ "    int " ++ varName var ++ " = 0;\n"
    put (env { declared = Set.insert var (declared env) })
  tell $ "    printf(\"" ++ prompt ++ "\");\n"
  tell $ "    scanf(\"%d\", &" ++ varName var ++ ");\n"

codeGenS (Def var expr stmts) = do
  cExpr <- codeGenE expr
  tell "    {\n"

  tell $ "    const int " ++ varName var ++ " = " ++ cExpr ++ ";\n"
  mapM_ codeGenS stmts
  tell "    }\n" 

-- Gera uma expressão C a partir de  E2.
codeGenE :: E2 -> CgM String
codeGenE (LVal (VInt i)) = return $ show i
codeGenE (LVal (VStr s)) = return $ "\"" ++ s ++ "\""
codeGenE (LVar var)      = return $ varName var
codeGenE (LAdd e1 e2)    = do
  c1 <- codeGenE e1
  c2 <- codeGenE e2
  return $ "(" ++ c1 ++ " + " ++ c2 ++ ")"
codeGenE (LMinus e1 e2)  = do
  c1 <- codeGenE e1
  c2 <- codeGenE e2
  return $ "(" ++ c1 ++ " - " ++ c2 ++ ")"
codeGenE (LMul e1 e2)    = do
  c1 <- codeGenE e1
  c2 <- codeGenE e2
  return $ "(" ++ c1 ++ " * " ++ c2 ++ ")"
codeGenE (LDiv e1 e2)    = do
  c1 <- codeGenE e1
  c2 <- codeGenE e2
  return $ "(" ++ c1 ++ " / " ++ c2 ++ ")"