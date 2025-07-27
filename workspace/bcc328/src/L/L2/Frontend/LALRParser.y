{
module L.L2.Frontend.LALRParser where

import L.L2.Frontend.Lexer (Token(..), Lexeme(..))
import L.L2.Frontend.Syntax (L2(..), S2(..), E2(..))
import Utils.Var (Var(..))
import Utils.Value (Value(..))
import Control.Monad (void)
}

%name parser
%tokentype { Token }
%error { parseError }

%left '+' '-'
%left '*' '/'

%token
    var       { Token _ (TId $$) }
    num       { Token _ (TNumber $$) }
    str       { Token _ (TString $$) }
    ":="      { Token _ TAssign }
    "read"    { Token _ TRead }
    "print"   { Token _ TPrint }
    '('       { Token _ TLParen }
    ')'       { Token _ TRParen }
    ','       { Token _ TComma }
    ';'       { Token _ TSemicolon }
    '+'       { Token _ TPlus }
    '-'       { Token _ TMinus }
    '*'       { Token _ TTimes }
    '/'       { Token _ TDiv }
    "def"     { Token _ TDef }
    "in"      { Token _ TIn }
    "end"     { Token _ TEnd }

%%

L2 : Stmt L2    { L2 ($1 : let (L2 ss) = $2 in ss) }
   |            { L2 [] }

Stmt : var ":=" Expr ';'               { LAssign (Var $1) $3 }
     | "read" '(' str ',' var ')' ';'  { LRead $3 (Var $5) }
     | "print" '(' Expr ')' ';'        { LPrint $3 }
     | "def" var ":=" Expr "in" L2 "end" { Def (Var $2) $4 (let (L2 ss) = $6 in ss) }

Expr : Expr '+' Term   { LAdd $1 $3 }
     | Expr '-' Term   { LMinus $1 $3 }
     | Term            { $1 }

Term : Term '*' Factor { LMul $1 $3 }
     | Term '/' Factor { LDiv $1 $3 }
     | Factor          { $1 }

Factor : BasicUnit     { $1 }

BasicUnit : num        { LVal (VInt $1) }
          | var        { LVar (Var $1) }
          | str        { LVal (VStr $1) }
          | '(' Expr ')' { $2 }

{
parseError :: [Token] -> a
parseError [] = error "Erro de sintaxe no final do arquivo."
parseError (t:_) = 
    let (Token (line, col) lexeme) = t
    in error $ "Erro de sintaxe na linha " ++ show line ++ ", coluna " ++ show col ++ " perto de " ++ show lexeme
}