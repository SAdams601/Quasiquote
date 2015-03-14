{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, ScopedTypeVariables, ExistentialQuantification #-}
module Lam where

import Language.Haskell.TH.Quote as TH
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax as TH
import Data.Generics
import Data.Set hiding (foldl)
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import System.IO

data Var = V String
         deriving (Eq, Show, Typeable, Data, Ord)

data Exp = Var Var
         | Lam Var Lam.Exp
         | App Lam.Exp Lam.Exp
         deriving (Show, Typeable, Data)

-- Evaluator Work

integers :: [Integer]
integers = [1..]

allBinders :: [Var]
allBinders = [V [x] | x <- ['a'..'z']] ++
             [V (x:show i) | x <- ['a'..'z'],
                             i <- integers]
allBinders' :: Set Var
allBinders' = fromList allBinders

free :: Lam.Exp -> [Var]
free = elems . free'

free' :: Lam.Exp -> Set Var
free' (Var v) = singleton v
free' (Lam v e) = free' e \\ (singleton v)
free' (App e1 e2) = free' e1 `union` free' e2

occurs :: Lam.Exp -> [Var]
occurs = elems . occurs'

occurs' :: Lam.Exp ->  Set Var
occurs' (Var v) = singleton v
occurs' (Lam v e) = insert v (occurs' e)
occurs' (App e1 e2) = occurs' e1 `union` occurs' e2

subst :: Lam.Exp -> Var -> Lam.Exp -> Lam.Exp
subst e x y = subst' (allBinders' \\ occurs' e `union` occurs' y) e x y
  where
    subst' :: Set Var -> Lam.Exp -> Var -> Lam.Exp -> Lam.Exp
    subst' _ e@(Var v) x y
      | v == x = y
      | otherwise = e
    subst' fresh e@(Lam v body) x y
      | v == x             = e
      | member v (free' y) = Lam v' (subst' fresh' body' x y)
      | otherwise          = Lam v' (subst' fresh body x y)
        where
          v' :: Var
          fresh' :: Set Var
          (v':lstFresh) = elems fresh
          fresh' = fromList lstFresh

          body' :: Lam.Exp
          body' = subst' (error "Fresh variables not so fresh")
                         body v (Var v')
    subst' fresh (App e1 e2) x y =
      let e1' = subst' fresh e1 x y
          e2' = subst' fresh e2 x y
          in App e1' e2'

eval :: Lam.Exp -> Lam.Exp
eval e@(Var _) = e
eval e@(Lam _ _) = e
eval (App e1 e2) =
  case eval e1 of
    Lam v body -> eval (subst body v e2)
    e1'        -> App e1' (eval e2)

parseAndEval :: String -> IO ()
parseAndEval s = do 
  p <- Lam.parse s
  let e = eval p
  hPrint stdout e

--Parser Work


parens p = between (symbol "(") (symbol ")") p
whiteSpace = many $ oneOf " \t"
small = lower <|> char '_'
large = upper
idchar = small <|> large <|> digit <|> char '\''
lexeme p = do x <- p
              whiteSpace
              return x
symbol name = lexeme $ string name

ident :: CharParser () String
ident = lexeme $
        do c <- small
           cs <- many idchar
           return $ c : cs

var :: CharParser () Var
var = do v <- ident
         return $ V v

exp :: CharParser () Lam.Exp
exp = do es <- many1 aexp
         return $ foldl1 App es

aexp :: CharParser () Lam.Exp
aexp = (try $ do v <- var
                 return $ Var v)
       <|> do symbol "\\"
              v <- var
              symbol "."
              e <- Lam.exp
              return $ Lam v e
       <|> parens Lam.exp

parse :: Monad m => String -> m Lam.Exp
parse s =
  case runParser p () "" s of
    Left err -> fail $ show err
    Right e -> return e
  where
    p = do e <- Lam.exp
           eof
           return e

-- Quasiquote stuff
lame :: (String,Int,Int) -> String -> TH.ExpQ
lame _ s = Lam.parse s >>= dataToExpQ

lamp :: (String,Int,Int) -> String -> TH.PatQ
lamp _ s = Lam.parse s >>= dataToPatQ