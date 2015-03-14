{-# LANGUAGE DeriveDataTypeable, QuasiQuotes #-}
module Expr where

import Data.Generics
import Text.ParserCombinators.Parsec.Char
import Text.Parsec
import Language.Haskell.TH.Quote as TH
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax as TH

  
data Expr = IntExpr Integer
          | AntiIntExpr String
          | BinOpExpr  BinOp Expr Expr
          | AntiExpr String
          deriving (Show, Typeable, Data)

data BinOp = AddOp
           | SubOp
           | MulOp
           | DivOp
           deriving(Show, Typeable, Data)

eval :: Expr -> Integer
eval (IntExpr n) = n
eval (BinOpExpr op e1 e2) = (opToFun op) (eval e1) (eval e2)
  where opToFun AddOp = (+)
        opToFun SubOp = (-)
        opToFun MulOp = (*)
        opToFun DivOp = div

small = lower <|> char '_'
large = upper
idchar = small  <|> large <|> digit <|> char '\''

lexeme p = do {x<-p;spaces;return x}
symbol name = lexeme (string name)
parens p = between (symbol "(") (symbol ")") p

expr :: CharParser st Expr
expr = term `chainl1` addop

term :: CharParser st Expr
term = factor `chainl1` mulop

factor :: CharParser st Expr
factor = parens expr <|> integer <|> try antiIntExpr <|> antiExpr

mulop = do {symbol "*"; return $ BinOpExpr MulOp}
    <|> do {symbol "/"; return $ BinOpExpr DivOp}


addop = do {symbol "+"; return $ BinOpExpr AddOp}
    <|> do {symbol "-"; return $ BinOpExpr SubOp}

integer :: CharParser st Expr
integer = lexeme $ do{ds <- many1 digit; return $ IntExpr (read ds) }

ident :: CharParser s String
ident = do{ c <- small; cs <- many idchar; return (c:cs) }

antiIntExpr = lexeme $ do{symbol "$int:"; id <- ident; return $ AntiIntExpr id}
antiExpr = lexeme $ do {symbol "$"; id <- ident; return $ AntiExpr id }

parseExpr :: Monad m => (String, Int, Int) -> String -> m Expr
parseExpr (file, line, col) s =
  case runParser p () "" s of
    Left err -> fail $ show err
    Right e -> return e
  where
    p = do pos <- getPosition
           setPosition $
             (flip setSourceName) file $
             (flip setSourceLine) line $
             (flip setSourceColumn) col $
             pos
           spaces
           e <- expr
           eof
           return e
           
quoteExprExp :: String -> TH.ExpQ
quoteExprExp s =
  do
    loc <- TH.location
    let pos = (TH.loc_filename loc,
               fst (TH.loc_start loc),
               snd (TH.loc_start loc))
    expr <- parseExpr pos s
    dataToExpQ (const Nothing `extQ` antiExprExp) expr

antiExprExp :: Expr -> Maybe (TH.Q TH.Exp)
antiExprExp (AntiIntExpr v) = Just $ TH.appE (TH.conE (TH.mkName "IntExpr"))
                                             (TH.varE (TH.mkName v))
antiExprExp (AntiExpr v)    = Just $ TH.varE (TH.mkName v)
antiExprExp _               = Nothing

quoteExprPat :: String -> TH.PatQ
quoteExprPat s =
  do
    loc <- TH.location
    let pos = (TH.loc_filename loc,
               fst (TH.loc_start loc),
               snd (TH.loc_start loc))
    expr <- parseExpr pos s
    dataToPatQ (const Nothing `extQ` antiExprPat) expr    

antiExprPat :: Expr -> Maybe (TH.Q TH.Pat)
antiExprPat (AntiIntExpr v) = Just $ TH.conP (TH.mkName "IntExpr")
                                             [TH.varP (TH.mkName v)]
antiExprPat (AntiExpr v)    = Just $ TH.varP (TH.mkName v)
antiExprPat _               = Nothing

ex :: QuasiQuoter
ex = QuasiQuoter { quoteExp = quoteExprExp,
                     quotePat = quoteExprPat,
                     quoteType = undefined,
                     quoteDec = undefined
                   }
