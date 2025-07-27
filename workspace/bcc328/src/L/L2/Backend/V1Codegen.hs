module L.L2.Backend.V1Codegen where

import Control.Monad.Writer

import L.L2.Frontend.Syntax
import Utils.Value
import Utils.Var
import V.V1.Instr (Code, Instr(..))

type CgM = Writer Code

v1Codegen :: L2 -> Code
v1Codegen (L2 stmts) = execWriter (mapM_ codeGenS stmts) ++ [Halt]

-- Gera código para uma expressão
codeGenE :: E2 -> CgM ()
codeGenE (LVal val) = tell [Push val]
codeGenE (LVar var) = tell [Load var]
codeGenE (LAdd e1 e2)  = codeGenE e1 >> codeGenE e2 >> tell [Add]
codeGenE (LMul e1 e2)  = codeGenE e1 >> codeGenE e2 >> tell [Mul]
codeGenE (LMinus e1 e2) = codeGenE e1 >> codeGenE e2 >> tell [Sub]
codeGenE (LDiv e1 e2) = codeGenE e1 >> codeGenE e2 >> tell [Div]

-- Gera código para S2.
codeGenS :: S2 -> CgM ()
codeGenS (LPrint expr) = codeGenE expr >> tell [Print]

codeGenS (LAssign var expr) = do
  codeGenE expr
  tell [Store var]

codeGenS (LRead _prompt var) = do
  tell [Input]
  tell [Store var]

codeGenS (Def var expr stmts) = do
  codeGenE expr
  tell [Store var]

  mapM_ codeGenS stmts
