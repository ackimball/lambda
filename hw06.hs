--Various functions taken/adapted from https://github.com/JakeWheat/intro_to_parsing

module Hw06 where

import System.Environment
import Control.Applicative ((<*), (*>), (<*>), (<$>))
import Data.Char
import Text.Parsec
import Text.Parsec.String (parseFromFile)
import FunctionsAndTypesForParsing
import Control.Monad (void)
import qualified Data.Map as Map
import Data.Map (Map, findWithDefault, (!))
import qualified Data.Set as Set
import Data.Set (Set)
import System.Exit
import System.Environment


type VarName = String

type Store = Map VarName LamExp

data LamExp =
    Var VarName
  | App LamExp LamExp
  | Lam VarName LamExp
  deriving Eq

data Stmt =
      Let VarName LamExp
    | Exp LamExp
    | Seq Stmt Stmt
   deriving (Show, Eq)



instance Show LamExp where
  show = show' 0 where
    show' _ (Var v) = v
    show' z (App la1 la2)
     | z < 1 = show' 1 la1 ++ " " ++ show' 1 la2
     | otherwise = "(" ++ show' 1 la1 ++ " " ++ show' 1 la2 ++ ")"
    show' z (Lam x la)
     | z < 1 = "lambda " ++ show' 1 (Var x) ++ ". " ++ show' 1 la
     | otherwise = "(" ++ "lambda " ++ show' 1 (Var x) ++ ". " ++ show' 1 la ++ ")"


test1 = Var "x"
test2 = Var "y"
test3 = Lam "x" test2
test4 = Lam "y" (App test1 test2)

type Parser = Parsec String ()


--program ::= statement (;statement)+ ;?
--statement ::= let x = e | e

parens :: Parser a -> Parser a
parens p = (char '(' *> p) <* char ')'

dot :: Parser Char
dot = char '.'

varExamples :: [(String,String)]
varExamples = [("test", "test")
               ,("_stuff", "_stuff")
               ,("_1234", "_1234")]

keywords :: [String]
keywords = ["lambda","let"]

isKeyword = (`elem` keywords)

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           ws
           return x

var :: Parser String
var = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> firstChar <*> many nonFirstChar
    check x = if x `elem` keywords
                then fail $ "keyword " ++ show x ++ " cannot be a var"
                else return x

kw :: String -> Parser String
kw s = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> firstChar <*> many nonFirstChar
    check x = if (not (s == x))
                then fail $ show x ++ ", I think you meant " ++ show s
                else return x

ws :: Parser ()
ws = void $ many $ oneOf " \n\t"

lexp ::Parser LamExp
lexp = ws *> (chainl1 (lamP <|> varP <|> (parens lexp)) (ws *> op))

varP :: Parser LamExp
varP =  Var <$> (ws *> var)

firstChar = satisfy (\a -> isLetter a || a == '_')
nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

lamP :: Parser LamExp
lamP = try oneArg <|> try multArgs
        where
          oneArg = Lam <$> (ws *> ((kw "lambda") *> var <* dot)) <*> lexp
          multArgs = Lam <$> (ws *> ((kw "lambda") *> var)) <*> lamP2

lamP2 :: Parser LamExp
lamP2 = try moreArgs <|> lastArg
        where
          lastArg = Lam <$> (var <* dot) <*> lexp
          moreArgs = Lam <$> var <*> lamP2

op :: Parser (LamExp -> LamExp -> LamExp)
op =
  do return app

app :: LamExp -> LamExp -> LamExp
app x y = App x y

sc :: Parser Char
sc = char ';'

nl :: Parser Char
nl = ws *> satisfy (=='\n')

program :: Parser Stmt
program = seqParser

seqParser :: Parser Stmt
seqParser = ws *> (try (Seq <$> (stmtParser <* sc) <*> seqParser) <|> try stmtParser)

stmtParser :: Parser Stmt
stmtParser = try expCase <|> try letCase
          where
             expCase = Exp <$> lexp
             letCase = Let <$> ((kw "let") *> var <* (char '=')) <*> lexp

--Substitution
subst :: LamExp -> VarName -> LamExp -> LamExp
subst v@(Var y) x e = if (y == x) then e else v
subst (App e1 e2) x e3 = app (subst e1 x e3) (subst e2 x e3)
subst (Lam y e1) x e2 = if y == x then (subst e1 x e2) else (Lam y (subst e1 x e2))

-- EXAMPLE of how interpreter should work
-- If we parse test2, we get:
-- Seq (Let "zero" (Lam "s" (Lam "z" (Var "z")))) (Let "succ" (Lam "n" (Lam "s" (Lam "z" (App (Var "s") (App (App (Var "n") (Var "s")) (Var "z")))))))
-- This is how we want that to be "interpreted":
-- (Lam "s" (Lam "z" (App (Var "s") (App (App (Lam "s" (Lam "z" (App (Var "s") (App (App (Lam "s" (Lam "z" (Var "z"))) (Var "s")) (Var "z"))))) (Var "s")) (Var "z")))))
-- Here are the steps:
-- 1. evalStmt to get ride of the Seq, Let, etc. (done except for Exp, see comment)
-- 2. evalLam to make substitutions that go
--   from:
--   lambda s (lambda z. z) (lambda n . (lambda s. (lambda z (s ((n s) z)))))
--   to:
--   lambda s z. s ((lambda s z. s ((lambda s z. z) s z)) s z)


-- Interpreter for LC
-- So this needs to use substitution somehow
evalLam :: Store -> LamExp -> LamExp
evalLam st (Var x) = findWithDefault (Var x) x st
evalLam st (Lam x la) = Lam x (evalLam st la)
evalLam st (App l1@(Lam v1 e1) l2) = case l2 of
                              (Lam var expr) -> subst l1 v1 l2
                              e@_ -> evalLam st (app l1 (evalLam st l2))
evalLam st (App e@_ l2) = evalLam st (app (evalLam st e) l2  )

-- Interpreter for Statements
evalStmt :: Store -> Stmt -> Store
evalStmt st (Let x l) = Map.insert x l st
evalStmt st (Exp l) = st
evalStmt st (Seq s1 s2) = (evalStmt (evalStmt st s1) s2)

evalStmt2 :: [LamExp] -> Stmt -> [LamExp]
evalStmt2 l (Let x a) = l
evalStmt2 l (Exp a) = (a:l)
evalStmt2 l (Seq s1 s2) = (evalStmt2 (evalStmt2 l s1) s2)

evalAll :: [LamExp] -> Store -> String
evalAll [] _ = "Running program"
evalAll (l:ls) st = (evalAll ls st) ++ "\n" ++ (show (evalLam st l))


evalAllWT :: [LamExp] -> Store -> String
evalAllWT [] _ = "Running program"
evalAllWT (l:ls) st = if (isClosed (evalLam st l)) then (evalAllWT ls st) ++ "\n" ++ (show (evalLam st l))
                    else error ((show (fv (evalLam st l))) ++ " are free variables in " ++ (show l))

--Checking for free variables
--(will be used with the -c flag)
fv :: LamExp -> Set VarName
fv (Var x) = Set.singleton x
fv (App e1 e2) = Set.union (fv e1) (fv e2)
fv (Lam x e) = Set.difference (fv e) (Set.singleton x)

--e is closed IFF fv(e) = empty set
isClosed :: LamExp -> Bool
isClosed e = fv e == Set.empty


main :: IO ()
main = getArgs >>= par


par ["-h"] = usage >> exit
par ["-c", fs] = do
  prog <- (parseFromFile program (fs))
  case prog of
        Right e1 -> let stores = (evalStmt s e1)
                        lams = (evalStmt2 g e1) in
                        putStr (evalAllWT lams stores)

        Left e2  -> error (show e2)
        where s = Map.empty
              g = []
par ["-n", fs] = do
  prog <- (parseFromFile program (fs))
  case prog of
        Right e1 -> let stores = (evalStmt s e1)
                        lams = (evalStmt2 g e1) in
                        putStr (evalAllWT lams stores)

        Left e2  -> error (show e2)
        where s = Map.empty
              g = []
par ["-cn", fs] = do
  prog <- (parseFromFile program (fs))
  case prog of
        Right e1 -> let stores = (evalStmt s e1)
                        lams = (evalStmt2 g e1) in
                        putStr (evalAllWT lams stores)

        Left e2  -> error (show e2)
        where s = Map.empty
              g = []
par ["-nc", fs] = do
  prog <- (parseFromFile program (fs))
  case prog of
        Right e1 -> let stores = (evalStmt s e1)
                        lams = (evalStmt2 g e1) in
                        putStr (evalAllWT lams stores)

        Left e2  -> error (show e2)
        where s = Map.empty
              g = []
par ["-c"] = do
  input <- getContents
  case (regularParse program input) of
                   Right e1 -> let s = (evalStmt s e1)
                                   g = (evalStmt2 g e1) in
                                   putStr (evalAll g s)
                   Left e2  -> error (show e2)
                   where s = Map.empty
                         g = []
par ["-n"] = do
  input <- getContents
  case (regularParse program input) of
                   Right e1 -> let s = (evalStmt s e1)
                                   g = (evalStmt2 g e1) in
                                   putStr (evalAll g s)
                   Left e2  -> error (show e2)
                   where s = Map.empty
                         g = []
par ["-cn"] = do
  input <- getContents
  case (regularParse program input) of
                   Right e1 -> let s = (evalStmt s e1)
                                   g = (evalStmt2 g e1) in
                                   putStr (evalAll g s)
                   Left e2  -> error (show e2)
                   where s = Map.empty
                         g = []
par ["-nc"] = do
  input <- getContents
  case (regularParse program input) of
                   Right e1 -> let s = (evalStmt s e1)
                                   g = (evalStmt2 g e1) in
                                   putStr (evalAll g s)
                   Left e2  -> error (show e2)
                   where s = Map.empty
                         g = []
par _ = errorExit


usage =  do putStrLn "interp [OPTIONS] FILE (defaults to -, for stdin)"
            putStrLn "  lambda calculus interpreter"
            putStrLn "  -c --check    Check scope"
            putStrLn "  -n --numeral  Convert final Church numeral to a number"
            putStrLn "  -? --help     Display help message]"
exit = exitWith ExitSuccess
errorExit = exitWith (ExitFailure 1)

-- main = do
--     a <- getArgs
--     case a of
--       [str] -> do
--             prog <- (parseFromFile program str)
--             case prog of
--                   Right e1 -> let stores = (evalStmt s e1)
--                                   lams = (evalStmt2 g e1) in
--                                   putStr (evalAllWT lams stores)
--
--                   Left e2  -> error (show e2)
--                   where s = Map.empty
--                         g = []
--
--       _ -> do
--              input <- getContents
--              case (regularParse program input) of
--                   Right e1 -> let s = (evalStmt s e1)
--                                   g = (evalStmt2 g e1) in
--                                   putStr (evalAll g s)
--                   Left e2  -> error (show e2)
--                   where s = Map.empty
--                         g = []
