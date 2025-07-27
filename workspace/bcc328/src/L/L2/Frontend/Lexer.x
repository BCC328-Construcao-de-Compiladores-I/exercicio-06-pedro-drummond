{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module L.L2.Frontend.Lexer (Token (..), Lexeme (..), lexer) where 
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$char  = [^"\n]  -- Qualquer caractere exceto aspas duplas e nova linha

@number     = $digit+
@id         = $alpha ($alpha | $digit)*
@string     = \" $char* \"

tokens :-
      $white+       ;
      "//" .*       ;
      @number        {mkNumber}
      @string        {mkString}

      "("            {simpleToken TLParen}
      ")"            {simpleToken TRParen}
      "+"            {simpleToken TPlus}
      "*"            {simpleToken TTimes}
      "/"            {simpleToken TDiv}
      ":="           {simpleToken TAssign}
      ";"            {simpleToken TSemicolon}  
      ","            {simpleToken TComma}   
      "-"            {simpleToken TMinus} 

      "read"         {simpleToken TRead}
      "print"        {simpleToken TPrint}

      "def"          {simpleToken TDef}
      "in"           {simpleToken TIn}
      "end"          {simpleToken TEnd}

      @id            {mkId}      

{
data Token
  = Token {
      pos :: (Int, Int)
    , lexeme :: Lexeme
    } deriving (Eq, Ord, Show)

data Lexeme
  = TAssign     -- :=
  | TRead       -- read
  | TPrint      -- print
  | TPlus       -- +
  | TMinus      -- -
  | TTimes      -- *
  | TDiv        -- /
  | TLParen     -- (
  | TRParen     -- )
  | TNumber Int
  | TString String
  | TId String  -- Identificador
  | TSemicolon  -- ;
  | TComma      -- ,
  | TDef        -- def
  | TIn         -- in
  | TEnd        -- end
  | TEOF
  deriving (Eq, Ord, Show)



position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x,y)

mkNumber :: AlexPosn -> String -> Token
mkNumber p s = Token (position p) (TNumber $ read s)

mkString :: AlexPosn -> String -> Token
mkString p s = 
  case s of
    '"':rest -> Token (position p) (TString (init rest))
    _        -> error "mkString: string mal formada"

mkId :: AlexPosn -> String -> Token
mkId p s = Token (position p) (TId s)

simpleToken :: Lexeme -> AlexPosn -> String -> Token
simpleToken lx p _ = Token (position p) lx

lexer :: String -> [Token]
lexer = alexScanTokens
}