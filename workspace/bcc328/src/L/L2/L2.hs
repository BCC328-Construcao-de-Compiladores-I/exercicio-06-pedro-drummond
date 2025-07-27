import L.L2.Backend.CCodegen
import L.L2.Backend.V1Codegen
import L.L2.Interpreter.Interp
import L.L2.Frontend.Lexer 
import L.L2.Frontend.Syntax
import L.L2.Frontend.LALRParser (parser)
import L.L2.Frontend.TypeCheck (typeCheck)
import Utils.Pretty
import Utils.Repl
import Utils.Value
import V.V0.Instr

import L.L2.Backend.V1Codegen (v1Codegen)
import Utils.Pretty (pretty)
import System.FilePath (replaceExtension)

import System.Environment
import System.FilePath
import System.Process 

import L.L2.Backend.CCodegen (cCodegen)
import System.Process (callCommand)

main :: IO ()
main = do
  args <- getArgs
  let opts = parseOptions args
  runWithOptions opts

-- running the compiler / interpreter

runWithOptions :: [Option] -> IO ()
runWithOptions opts = case opts of
  [Lexer file] ->
    lexerOnly file
  [Parser file] ->
    parserOnly file
  [Interpret file] ->
    interpret file
  [VM file] ->
    v1Compiler file
  [C file] ->
    cCompiler file
  _ -> helpMessage


-- Implement the function to do lexical analysis for L2 programs and outputs the tokens

lexerOnly :: FilePath -> IO ()
lexerOnly file = do
  content <- readFile file
  let tokens = lexer content
  mapM_ printToken tokens

printToken :: Token -> IO ()
printToken (Token (l, c) lexeme) =
  putStrLn $ case lexeme of
    TAssign     -> "Atribuição :=  | row:" ++ show l ++ " col:" ++ show c
    TRead       -> "Comando  read | row:" ++ show l ++ " col:" ++ show c
    TPrint      -> "Comando  print | row:" ++ show l ++ " col:" ++ show c
    TDef        -> "Comando  def | row:" ++ show l ++ " col:" ++ show c
    TIn         -> "Comando  in | row:" ++ show l ++ " col:" ++ show c
    TEnd        -> "Comando  end | row:" ++ show l ++ " col:" ++ show c
    TPlus       -> "Operador + | row:" ++ show l ++ " col:" ++ show c
    TMinus      -> "Operador - | row:" ++ show l ++ " col:" ++ show c
    TTimes      -> "Operador * | row:" ++ show l ++ " col:" ++ show c
    TLParen     -> "Parêntesis ( | row:" ++ show l ++ " col:" ++ show c
    TRParen     -> "Parêntesis ) | row:" ++ show l ++ " col:" ++ show c
    TSemicolon  -> "Ponto e vírgula ; | row:" ++ show l ++ " col:" ++ show c
    TComma      -> "Virgula , | row:" ++ show l ++ " col:" ++ show c
    TNumber n   -> "Número " ++ show n ++ " | row:" ++ show l ++ " col:" ++ show c
    TString s   -> "String \"" ++ s ++ "\" | row:" ++ show l ++ " col:" ++ show c
    TId s       -> "Identificador " ++ s ++ " | row:" ++ show l ++ " col:" ++ show c
    TEOF        -> "End of file | row:" ++ show l ++ " col:" ++ show c


-- Implement the function to do syntax analysis for L2 programs and outputs the syntax tree

parserOnly :: FilePath -> IO ()
parserOnly file = do
  content <- readFile file
  let tokens = lexer content
  let ast = parser tokens 
  putStrLn "LALR"
  putStrLn (show (ppr ast))

-- Implement the whole interpreter pipeline: lexical and syntax analysis and then interpret the program

interpret :: FilePath -> IO ()
interpret file = do
  content <- readFile file
  let tokens = lexer content
  let ast = parser tokens
  -- análise semântica ANTES 
  case typeCheck ast of
    Left err -> putStrLn err 
    Right checkedAst -> interpL2 checkedAst --interpreta

-- Implement the whole compiler pipeline: lexical, syntax and semantic analysis and then generate v1 instructions from the program.

v1Compiler :: FilePath -> IO ()
v1Compiler file = do
  content <- readFile file
  let tokens = lexer content
  let ast = parser tokens
  case typeCheck ast of
    Left err -> putStrLn err
    Right checkedAst -> do
      let code = v1Codegen checkedAst
      let out_file = replaceExtension file ".v1"
      writeFile out_file (pretty code)
      putStrLn $ "Código V1 escrito em: " ++ out_file

-- Implement the whole executable compiler, using C source and GCC.

cCompiler :: FilePath -> IO ()
cCompiler file = do
  content <- readFile file
  let tokens = lexer content
  let ast = parser tokens
  case typeCheck ast of
    Left err -> putStrLn err
    Right checkedAst -> do
      let c_code = cCodegen checkedAst
      let c_file = replaceExtension file ".c"
      let out_file = dropExtension file
      writeFile c_file c_code
      putStrLn $ "Código C gerado em: " ++ c_file
      -- Constrói e executa o comando GCC
      let gcc_command = "gcc " ++ c_file ++ " -o " ++ out_file
      putStrLn $ "Executando: " ++ gcc_command
      callCommand gcc_command
      putStrLn $ "Executável gerado em: " ++ out_file

-- help message

helpMessage :: IO ()
helpMessage
  = putStrLn $ unlines [ "L2 language"
                       , "Usage: l2 [--lexer-only | --parse-only | --interpret | --help]"
                       , "--lexer-only: does the lexical analysis of the input program."
                       , "--parse-only: does the syntax analysis of the input program."
                       , "--interpret: does the syntax and semantic analysis and interpret the input program."
                       , "--v1: does the syntax and semantic analysis and then generates V1 code."
                       , "--c: does the syntax and semantic analysis, generates C code and uses GCC to generate an executable."
                       , "--help: prints this help message."
                       ]

-- parse command line arguments

data Option
  = Help
  | Lexer FilePath
  | Parser FilePath
  | Interpret FilePath
  | VM FilePath
  | C FilePath
  deriving (Eq, Show)

parseOptions :: [String] -> [Option]
parseOptions args =
  case args of
    ("--lexer-only" : arg : _) -> [Lexer arg]
    ("--parse-only" : arg : _) -> [Parser arg]
    ("--interpret" : arg : _) -> [Interpret arg]
    ("--v1" : arg : _) -> [VM arg]
    ("--c" : arg : _) -> [C arg]
    _ -> [Help]
