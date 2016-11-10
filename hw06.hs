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
  | Lam VarName Type LamExp
  | If LamExp LamExp LamExp
  | Let VarName LamExp LamExp
  | LetRec VarName LamExp LamExp
  | Assign LamExp Type
  | Nat Int
  | True
  | False
  | Pair LamExp LamExp
  | Unop LamExp
  | Binop LamExp LamExp
  deriving (Show, Eq)


data Stmt =
      LetS VarName LamExp
    | Exp LamExp
    | Seq Stmt Stmt
   deriving (Show, Eq)

data Type  =
      IntT Int
    | BoolT Bool
    | FuncT Type Type
    | PairT Type Type
    deriving (Show, Eq)

-- instance Show LamExp where
--   show = show' 0 where
--     show' _ (Var v) = v
--     show' z (App la1 la2)
--      | z < 1 = show' 1 la1 ++ " " ++ show' 1 la2
--      | otherwise = "(" ++ show' 1 la1 ++ " " ++ show' 1 la2 ++ ")"
--     show' z (Lam x la)
--      | z < 1 = "lambda " ++ show' 1 (Var x) ++ ". " ++ show' 1 la
--      | otherwise = "(" ++ "lambda " ++ show' 1 (Var x) ++ ". " ++ show' 1 la ++ ")"
--
-- one = Lam "s" (Lam "z" (App (Var "s") (App (App (Lam "s" (Lam "z" (Var "z"))) (Var "s")) (Var "z"))))
--
-- asdf = (App (Var "s") (App (App (Lam "s" (Lam "z" (Var "z"))) (Var "s")) (Var "z")))
--
-- successor = (Lam "n" (Lam "s" (Lam "z" (App (Var "s") (App (App (Var "n") (Var "s")) (Var "z"))))))
-- zero = (Lam "s" (Lam "z" (Var "z")))
--
-- omega = (Lam "x" (App (Var "x") (Var "x")))

type Parser = Parsec String ()


--program ::= statement (;statement)+ ;?
--statement ::= let x = e | e

parens :: Parser a -> Parser a
parens p = (char '(' *> p) <* char ')'

dot :: Parser Char
dot = char '.'

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
ws = void $ many $ oneOf " \n"

-- lexp ::Parser LamExp
-- lexp = ws *> (chainl1 (lamP <|> varP <|> (parens lexp)) (ws *> op))
--
-- varP :: Parser LamExp
-- varP =  Var <$> (ws *> var)
--
firstChar = satisfy (\a -> isLetter a || a == '_')
nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')
--
-- lamP :: Parser LamExp
-- lamP = try oneArg <|> try multArgs
--         where
--           oneArg = Lam <$> (ws *> ((kw "lambda") *> var <* dot)) <*> lexp
--           multArgs = Lam <$> (ws *> ((kw "lambda") *> var)) <*> lamP2
--
-- lamP2 :: Parser LamExp
-- lamP2 = try moreArgs <|> lastArg
--         where
--           lastArg = Lam <$> (var <* dot) <*> lexp
--           moreArgs = Lam <$> var <*> lamP2
--
-- op :: Parser (LamExp -> LamExp -> LamExp)
-- op =
--   do return app
--
-- app :: LamExp -> LamExp -> LamExp
-- app x y = App x y
--
-- sc :: Parser Char
-- sc = ws *> (char ';') <* ws
--
-- nl :: Parser Char
-- nl = ws *> satisfy (=='\n')
--
-- program :: Parser Stmt
-- program = seqParser
--
-- seqParser :: Parser Stmt
-- seqParser = ws *> (try (Seq <$> (stmtParser) <*> seqParser) <|> try stmtParserEnd <|> try stmtParserEnd2)
--
-- stmtParser :: Parser Stmt
-- stmtParser = expCase <|> letCase
--           where
--              expCase = Exp <$> lexp <* sc
--              letCase = Let <$> ((kw "let") *> var <* (char '=')) <*> lexp <* sc
--
--
-- stmtParserEnd :: Parser Stmt
-- stmtParserEnd = expCase <|> letCase
--           where
--              expCase = Exp <$> lexp <* ws <* eof
--              letCase = Let <$> ((kw "let") *> var <* (char '=')) <*> lexp <* ws <* eof
--
-- stmtParserEnd2 :: Parser Stmt
-- stmtParserEnd2 = expCase <|> letCase
--           where
--              expCase = Exp <$> lexp <* sc <* eof
--              letCase = Let <$> ((kw "let") *> var <* (char '=')) <*> lexp <* sc <* eof
--
-- --Substitution
-- subst :: LamExp -> VarName -> LamExp -> LamExp
-- subst v@(Var y) x e = if (y == x) then e else v
-- subst (App e1 e2) x e3 = app (subst e1 x e3) (subst e2 x e3)
-- subst (Lam y e1) x e2 = if y == x then (Lam y e1) else (Lam y (subst e1 x e2))
--
-- -- Interpreter for LC
-- evalLam :: Store -> LamExp -> LamExp
-- --evalLam st (Var x) = (findWithDefault (Var x) x st)
-- evalLam st v@(Var x) = if ((findWithDefault (Var x) x st) == v) then v else (evalLam st (st ! x))
-- evalLam st (Lam x la) = Lam x (evalLam st la)
-- evalLam st (App l1@(Lam v1 e1) l2) = case l2 of
--                               (Lam var expr) -> (subst l1 v1 l2)
--                               (App a1 a2) -> evalLam st (app l1 (evalLam st l2))
--                               (Var v2) -> if ((findWithDefault (Var v2) v2 st) == l2) then (subst l1 v1 l2) else evalLam st (app l1 (evalLam st (st ! v2)))
-- --evalLam st (App l1@(Lam v1 e1) l2) = subst l1 v1 l2
-- evalLam st (App e@(Var x) l2) = if ((findWithDefault (Var x) x st) == e) then (app e (evalLam st l2)  ) else (evalLam st (app (evalLam st e) l2  ))
-- evalLam st a@(App e@(App a1 a2) l2) = case (evalLam st e) of
--                                             (App d1 d2) -> (app e (evalLam st l2))
--                                             _ -> evalLam st (app (evalLam st e) l2)
--
--
-- -- Interpreter for Statements
-- evalStmt :: Store -> Stmt -> Store
-- evalStmt st (Let x l) = Map.insert x (evalLam st l) st
-- evalStmt st (Exp l) = st
-- evalStmt st (Seq s1 s2) = (evalStmt (evalStmt st s1) s2)
--
-- evalStmt2 :: [LamExp] -> Stmt -> [LamExp]
-- evalStmt2 l (Let x a) = l
-- evalStmt2 l (Exp a) = (a:l)
-- evalStmt2 l (Seq s1 s2) = (evalStmt2 (evalStmt2 l s1) s2)
--
-- evalAll :: [LamExp] -> Store -> String
-- evalAll [] _ = ""
-- evalAll (l:ls) st = (evalAll ls st) ++ (show (evalLam st l)) ++ "\n"
--
-- evalAllWT :: [LamExp] -> Store -> String
-- evalAllWT [] _ = ""
-- evalAllWT (l:ls) st = if (isClosed (evalLam st l)) then (evalAllWT ls st) ++ (show (evalLam st l)) ++ "\n"
--                     else error ((show (fv (evalLam st l))) ++ " are free variables in " ++ (show l))
--
-- --Checking for free variables
-- --(will be used with the -c flag)
-- fv :: LamExp -> Set VarName
-- fv (Var x) = Set.singleton x
-- fv (App e1 e2) = Set.union (fv e1) (fv e2)
-- fv (Lam x e) = Set.difference (fv e) (Set.singleton x)
--
-- isClosed :: LamExp -> Bool
-- isClosed e = fv e == Set.empty
--
-- main :: IO ()
-- main = getArgs >>= par
-- par ["-d", fs] = do
--   prog <- (parseFromFile program (fs))
--   case prog of
--     Right e1 -> let stores = (evalStmt s e1)
--                     lams = (evalStmt2 g e1) in
--                     putStr ("The parsed bare expressions are: " ++ "\n" ++ (show lams) ++ "\n\n" ++ "The parsed vars are: " ++ "\n" ++ (show stores))
--
--     Left e2  -> error (show e2)
--     where s = Map.empty
--           g = []
-- par ["-d"] = do
--   input <- getContents
--   case (regularParse program input) of
--     Right e1 -> let stores = (evalStmt s e1)
--                     lams = (evalStmt2 g e1) in
--                     putStr ("The parsed bare expressions are: " ++ "\n" ++ (show lams) ++ "\n\n" ++ "The parsed vars are: " ++ "\n" ++ (show stores))
--
--     Left e2  -> error (show e2)
--     where s = Map.empty
--           g = []
-- par [fs] = do
--   prog <- (parseFromFile program (fs))
--   case prog of
--     Right e1 -> let stores = (evalStmt s e1)
--                     lams = (evalStmt2 g e1) in
--                     putStr (evalAll lams stores)
--     Left e2  -> error (show e2)
--     where s = Map.empty
--           g = []
-- par [] = do
--   input <- getContents
--   case (regularParse program input) of
--     Right e1 -> let stores = (evalStmt s e1)
--                     lams = (evalStmt2 g e1) in
--                     putStr (evalAll lams stores)
--     Left e2  -> error (show e2)
--     where s = Map.empty
--           g = []
-- par _ = errorExit
--
-- usage =  do putStrLn "interp [OPTIONS] FILE (defaults to -, for stdin)"
--             putStrLn "  lambda calculus interpreter"
--             putStrLn "  -c --check    Check scope"
--             putStrLn "  -n --numeral  Convert final Church numeral to a number"
--             putStrLn "  -? --help     Display help message]"
-- exit = exitWith ExitSuccess
-- errorExit = exitWith (ExitFailure 1)
--
-- Right plus = regularParse lexp "lambda m n. m lambda n. lambda s z. s (n s z) n"
--
--
-- test = "let zero = lambda s z. z; let succ = lambda n. lambda s z. s (n s z); succ zero"
--
-- Right testParsed = regularParse program test
-- testSt = (evalStmt Map.empty testParsed)
-- (testLam:rest)= (evalStmt2 [] testParsed)
-- lam1 = convert2 testSt testLam
-- num = convert lam1
--
-- testSt2 = Map.insert "omega" omega testSt
-- testSt3 = Map.insert "one" one testSt
-- testInf = evalLam testSt2 (App omega omega)
-- testPlus = evalLam testSt (App (App plus one) one)
