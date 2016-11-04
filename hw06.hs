--Various functions taken/adapted from https://github.com/JakeWheat/intro_to_parsing

module Hw06 where

import Control.Applicative ((<*), (*>), (<*>), (<$>))
import Data.Char
import Text.Parsec
import FunctionsAndTypesForParsing
import Control.Monad (void)


type VarName = String

data LamExp =
    Var VarName
  | App LamExp LamExp
  | Lam VarName LamExp
  deriving (Eq, Show)

-- instance Show LamExp where
--   show = show' 0 where
--     show' _ (Var v) = v
--     show' z (App la1 la2)
--      | z < 1 = show' 1 la1 ++ " " ++ show' 1 la2
--      | otherwise = "(" ++ show' 1 la1 ++ " " ++ show' 1 la2 ++ ")"
--     show' z (Lam x la)
--      | z < 1 = "lambda " ++ show' 1 (Var x) ++ ". " ++ show' 1 la
--      | otherwise = "(" ++ "lambda " ++ show' 1 (Var x) ++ ". " ++ show' 1 la ++ ")"


test1 = Var "x"
test2 = Var "y"
test3 = Lam "x" test2
test4 = Lam "y" (App test1 test2)

type Parser = Parsec String ()

--
-- newtype Parser a = Parser { parse :: String -> Maybe (a,String) }
--
-- instance Functor Parser where
--   fmap f p = Parser $ \s -> (\(a,c) -> (f a, c)) <$> parse p s
--
-- instance Applicative Parser where
--   pure a = Parser $ \s -> Just (a,s)
--   f <*> a = Parser $ \s ->
--     case parse f s of
--       Just (g,s') -> parse (fmap g a) s'
--       Nothing -> Nothing

-- instance Alternative Parser where
--   empty = Parser $ \s -> Nothing
--   l <|> r = Parser $ \s -> parse l s <|> parse r s

--
-- ensure :: (a -> Bool) -> Parsec a -> Parsec a
-- ensure p parser = Parser $ \s ->
--    case parse parser s of
--      Nothing -> Nothing
--      Just (a,s') -> if p a then Just (a,s') else Nothing
--
-- lookahead :: Parsec (Maybe Char)
-- lookahead = Parser f
--   where f [] = Just (Nothing,[])
--         f (c:s) = Just (Just c,c:s)
--
--
--
--
--
--
--
-- eof :: Parsec ()
-- eof = Parser $ \s -> if null s then Just ((),[]) else Nothing
--
--
-- ws :: Parsec ()
-- ws = pure () <* many (satisfy isSpace)

-- char :: Char -> Parser Char
-- char c = ws *> satisfy (==c)
--

-- parens :: Parsec a -> Parsec a
-- parens p = (char '(' *> p) <* char ')'
--
--


  --
  -- plus, minus, times :: Parser Char
  -- plus = char '+'
  -- minus = char '-'
  -- times = char '*'
  -- divs = char '/'
  -- --
  -- spaces :: Parser ()
  -- spaces = many (satisfy isSpace) *> pure ()
  -- --
  -- terms, factors, negs, atoms, aexp :: Parser AExp
  -- aexp = terms
  -- terms = Plus <$> factors <* plus <*> terms
  --         <|> (\a b -> Plus a (Neg b)) <$> factors <* minus <*> terms
  --         <|> factors
  -- factors = Times <$> negs <* times <*> factors
  --           <|> Div <$> negs <* divs <*> factors <|> negs
  -- negs = Neg <$> (minus *> atoms) <|> atoms
  -- atoms = Num <$> num <|> Var <$> var <|> (char '(' *> terms <* char ')')




  -- statement, statements, cSyntax :: Parser Stmt
  -- cSyntax = statements
  -- statements = Seq <$> statement <*> (statement <|> statements )
  -- statement = const Skip <$> kw "Skip"
  --          <|> Assign <$> var <* char '=' <*> aexp <* char ';'
  --          <|> If <$> (kw "if" *> bexp) <*> (char '{' *> statements  <* char '}')
  --          <*> (kw "else" *> (char '{' *> statements  <* char '}'))
  --          <|> While <$> (kw "while" *> bexp) <*> (char '{' *> statements <* char '}' <* char ';')
  --          <|> pure Skip <* ws
  --
  --
  --

  -- terms, factors, negs, atoms, aexp :: Parser AExp
  -- aexp = terms
  -- terms = Plus <$> factors <* plus <*> terms
  --         <|> (\a b -> Plus a (Neg b)) <$> factors <* minus <*> terms
  --         <|> factors
  -- factors = Times <$> negs <* times <*> factors
  --           <|> Div <$> negs <* divs <*> factors <|> negs
  -- negs = Neg <$> (minus *> atoms) <|> atoms
  -- atoms = Num <$> num <|> Var <$> var <|> (char '(' *> terms <* char ')')

--
-- spaces :: Parsec ()
-- spaces = many (satisfy isSpace) *> pure ()
--
--
dot :: Parser Char
dot = char '.'

-- lexp :: Parsec LamExp
-- lexp = undefined

-- lexp = lamP <|> (chainl appP)  <|> varP

--
-- var :: Parsec String
-- var = do
--    fc <- firstChar
--    rest <- many nonFirstChar
--    return (fc:rest)
--  where
--    firstChar = satisfy (\a -> isLetter a || a == '_')
--    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

varExamples :: [(String,String)]
varExamples = [("test", "test")
               ,("_stuff", "_stuff")
               ,("_1234", "_1234")]

-- ensure :: (a -> Bool) -> Parsec a -> Parsec a
-- ensure p parser = Parser $ \s ->
--    case parse parser s of
--      Nothing -> Nothing
--      Just (a,s') -> if p a then Just (a,s') else Nothing

-- str :: String -> Parser String
-- str s = ws *> loop s
--   where loop [] = pure []
--         loop (c:cs) = (:) <$> satisfy (==c) <*> loop cs


keywords :: [String]
keywords = ["lambda","let"]

isKeyword = (`elem` keywords)

-- kw :: String -> Parser String
-- kw s =  s <* ensure funct lookAhead
--       where funct Nothing = False
--             funct (Just x)  = x == ' '

-- var :: Parser String
-- var = ensure (not . (`elem` keywords)) (ws *> id)
--   where id = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)


--TODO: this definition of var does not check to make sure the var is not a keyword
var :: Parser String
var = do
   fc <- firstChar
   rest <- many nonFirstChar
   return (fc:rest)
 where
   firstChar = satisfy (\a -> isLetter a || a == '_')
   nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

ws :: Parser ()
ws = void $ many $ oneOf " \n\t"

varP :: Parser LamExp
varP =  Var <$> (ws *> var)



-- lamP :: Parser LamExp
-- lamP = Lam <$> (((ws *> var) *> ws *> var) <* dot) <*> (appP <|> lamP <|> varP)

-- appP :: Parser LamExp
-- appP = undefined

-- appP = App <$> (parens appP <|> lamP <|> varP) <*> (appP <|> lamP <|> varP)


-- varPEnd = varP <* ensure null lookahead

-- appP2 :: Parsec LamExp
-- appP2 = App <$> (varP <|> lamP <|> appP) <*> (parens appP <|> lamP <|> varPEnd <|> varP)

-- lamP :: Parsec LamExp
-- lamP = undefined

-- lamP = Lam <$> (((kw "lambda") *> var) <* dot) <*> (appP <|> lamP <|> varP)
--
-- first, second :: Parsec LamExp
-- firs = App <$> (second) <*> (second)
--     <|> Var <$> var
-- second = Lam <$> (((kw "lambda") *> var) <* dot) <*> (first)
--      <|> first
--      <|> Var <$> var

-- factor :: Parser LamExp
-- factor = Var <$> var <|>
--          App <$> lexp <*> lexp <|>
--          Lam <$> (kw "lambda" *> var <* dot) <*> lexp <|>
--          (char '(') *> lexp <* (char ')')


-- appP :: Parser LamExp
-- appP = App <$> lamP <*> lamP <|>
--
-- lamP :: Parser LamExp
-- lamP = Lam <$> (kw "lambda" *> var) <*> lexp
