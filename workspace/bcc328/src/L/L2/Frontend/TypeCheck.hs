module L.L2.Frontend.TypeCheck where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad (when)
import Data.List ((\\), elem)

import L.L2.Frontend.Syntax
import Utils.Var

typeCheck :: L2 -> Either String L2
typeCheck ast = case fst (runTcM initTcEnv (tcL2 ast)) of
  Left err -> Left err
  Right _  -> Right ast

type TcM a = ExceptT String (State TcEnv) a

data TcEnv
  = TcEnv {
      context :: [Var] -- Lista de variáveis imutáveis. 
    }

initTcEnv :: TcEnv
initTcEnv = TcEnv []

insertVar :: Var -> TcM ()
insertVar v = modify (\env -> env { context = v : context env })

removeVar :: Var -> TcM ()
removeVar v = modify (\env -> env { context = context env \\ [v] })

runTcM :: TcEnv -> TcM a -> ((Either String a), TcEnv)
runTcM env m = runState (runExceptT m) env

-- Verifica um programa L2.
tcL2 :: L2 -> TcM ()
tcL2 (L2 stmts) = mapM_ tcS2 stmts

-- Verifica um único comando S2.
tcS2 :: S2 -> TcM ()

-- verifica se a variável alvo é imutável.
tcS2 (LAssign var expr) = do
  env <- get
  when (var `elem` context env) $
    throwError $ "Erro Semântico: Tentativa de atribuir valor à variável imutável '" ++ show var ++ "'."
  tcE2 expr

tcS2 (LRead _ var) = do
  env <- get
  when (var `elem` context env) $
    throwError $ "Erro Semântico: Tentativa de ler valor para a variável imutável '" ++ show var ++ "'."

-- verifica a expressão.
tcS2 (LPrint expr) = tcE2 expr

tcS2 (Def var expr stmts) = do
  tcE2 expr          -- Verifica a expressão.
  insertVar var      -- Adiciona a variável ao contexto do escopo. 
  mapM_ tcS2 stmts   -- Verifica os comandos dentro do escopo.
  removeVar var      -- Remove a variável

-- Verifica uma expressão E2 
tcE2 :: E2 -> TcM ()
tcE2 (LVal _)      = return ()
tcE2 (LVar _)      = return ()
tcE2 (LAdd e1 e2)  = tcE2 e1 >> tcE2 e2
tcE2 (LMinus e1 e2)= tcE2 e1 >> tcE2 e2
tcE2 (LMul e1 e2)  = tcE2 e1 >> tcE2 e2
tcE2 (LDiv e1 e2)  = tcE2 e1 >> tcE2 e2