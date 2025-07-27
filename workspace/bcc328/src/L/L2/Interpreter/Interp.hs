module L.L2.Interpreter.Interp where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import L.L2.Frontend.Syntax
import Utils.Value
import Utils.Var

-- O ambiente do interpretador (InterpEnv) agora tem dois mapas:
-- sigma (σ): para variáveis mutáveis (atribuídas com :=)
-- phi (φ): para variáveis imutáveis (definidas com def) [cite: 79]
data InterpEnv = InterpEnv {
  sigma :: Map Var Value,
  phi   :: Map Var Value
}

-- Nosso monad do interpretador para gerenciar o estado e I/O.
type InterpM a = StateT InterpEnv IO a

-- Função principal que executa um programa L2.
interpL2 :: L2 -> IO ()
interpL2 (L2 stmts) = evalStateT (interpBlock stmts) initialEnv
  where
    initialEnv = InterpEnv Map.empty Map.empty

-- Executa um bloco de comandos.
interpBlock :: [S2] -> InterpM ()
interpBlock = mapM_ interpS

-- Executa um único comando.
interpS :: S2 -> InterpM ()
interpS (LAssign var expr) = do
  val <- evalE expr
  env <- get
  put (env { sigma = Map.insert var val (sigma env) })

interpS (LPrint expr) = do
  val <- evalE expr
  liftIO $ print val

interpS (LRead prompt var) = do
  liftIO $ putStr prompt
  input <- liftIO getLine
  let val = VInt (read input)
  env <- get
  put (env { sigma = Map.insert var val (sigma env) })

-- Lógica de escopo para 'def'.
interpS (Def var expr stmts) = do
  -- Avalia a expressão no escopo atual.
  val <- evalE expr
  
  -- Salva o ambiente de variáveis imutáveis (phi) atual.
  originalPhi <- gets phi

  -- Cria um novo ambiente 'phi' para o escopo do bloco, adicionando a nova variável.
  -- Corresponde a: φ' = φ[v ↦ n] [cite: 88]
  modify (\env -> env { phi = Map.insert var val (phi env) })

  -- Executa o bloco de comandos com o novo 'phi'.
  interpBlock stmts

  -- Restaura o ambiente 'phi' original, saindo do escopo.
  modify (\env -> env { phi = originalPhi })

-- Avalia uma expressão para um valor.
evalE :: E2 -> InterpM Value
evalE (LVal val) = return val

-- Lógica de busca de variáveis.
evalE (LVar var) = do
  env <- get
  -- A regra de busca é: primeiro no ambiente imutável (phi), depois no mutável (sigma).
  -- Esta lógica implementa as regras da semântica de L2[cite: 83, 84, 85, 86].
  case Map.lookup var (phi env) of
    Just val -> return val -- Encontrado em phi [cite: 85]
    Nothing  ->
      case Map.lookup var (sigma env) of
        Just val -> return val -- Encontrado em sigma [cite: 83]
        Nothing  -> error $ "Variável não definida: " ++ show var

evalE (LAdd e1 e2) = evalBinOp (+) e1 e2
evalE (LMinus e1 e2) = evalBinOp (-) e1 e2
evalE (LMul e1 e2) = evalBinOp (*) e1 e2
evalE (LDiv e1 e2) = evalBinOp div e1 e2

-- Função auxiliar para operadores binários.
-- Função auxiliar para operadores binários.
evalBinOp :: (Int -> Int -> Int) -> E2 -> E2 -> InterpM Value
evalBinOp op e1 e2 = do
  v1 <- evalE e1
  v2 <- evalE e2
  case (v1, v2) of
    (VInt i1, VInt i2) -> return $ VInt (op i1 i2)
    _ -> error "Operação aritmética em valores não inteiros."