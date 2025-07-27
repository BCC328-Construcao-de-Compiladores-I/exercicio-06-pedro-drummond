import L.L1.Backend.CCodegen
import L.L1.Backend.V1Codegen
import L.L1.Interpreter.Interp
import L.L1.Frontend.Lexer 
import L.L1.Frontend.RecursiveParser
import L.L1.Frontend.LALRParser (parser)
import L.L1.Frontend.Syntax
import Utils.Pretty
import Utils.Repl
import Utils.Value
import V.V0.Instr

import System.Environment
import System.FilePath
import System.Process 

main :: IO ()
main = do
  args <- getArgs
  let opts = parseOptions args
  runWithOptions opts 

-- running the compiler / interpreter 

runWithOptions :: [Option] -> IO ()
runWithOptions opts = case opts of 
  [Lexer file] ->
    alexBasedLexer file
  [Recursive file] -> 
    recursiveParser file
  [LALR file] -> 
    lalrParser file 
  _ -> helpMessage


-- Implement the function to do lexical analysis for L1 programs

alexBasedLexer :: FilePath -> IO ()
alexBasedLexer file = do
  content <- readFile file
  let tokens = lexer content
  mapM_ printToken tokens

printToken :: Token -> IO ()
printToken (Token (l, c) lexeme) =
  putStrLn $ case lexeme of
    TAssign     -> "Atribuição :=  | row:" ++ show l ++ " col:" ++ show c
    TRead       -> "Comando  read | row:" ++ show l ++ " col:" ++ show c
    TPrint      -> "Comando  print | row:" ++ show l ++ " col:" ++ show c
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


-- Implement the function to do syntax analysis using a recursive parser

recursiveParser :: FilePath -> IO ()
recursiveParser file = do
  content <- readFile file
  case l1Parser content of
    Left err -> putStrLn $ "Erro de sintaxe:\n" ++ err
    Right ast -> do
      putStrLn "Análise sintática concluída"
      putStrLn (show (ppr ast))

-- Implement the LALR parser 

lalrParser :: FilePath -> IO ()
lalrParser file = do
  content <- readFile file
  let tokens = lexer content
  let ast = parser tokens 
  putStrLn "LALR"
  putStrLn (show (ppr ast))

-- help message

helpMessage :: IO ()
helpMessage 
  = putStrLn $ unlines [ "L1 language" 
                       , "Usage: l1 [--lexer-only | --recursive | --help]"
                       , "--lexer-only: does the lexical analysis of the input programming using a Alex based lexer."
                       , "--recursive: does the syntax analysis using a recursive descendent Megaparsec parser."
                       , "--lalr: does the syntax analysis using a LALR parser."
                       , "--help: prints this help message."
                       ]

-- parse command line arguments 

data Option 
  = Help 
  | Lexer FilePath
  | Recursive FilePath
  | LALR FilePath 
  deriving (Eq, Show)

parseOptions :: [String] -> [Option]
parseOptions args = 
  case args of 
    ("--lexer-only" : arg : _) -> [Lexer arg]
    ("--recursive" : arg : _) -> [Recursive arg]
    ("--lalr" : arg : _) -> [LALR arg]
    _ -> [Help]
