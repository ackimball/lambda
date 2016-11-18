   --Various functions taken/adapted from https://github.com/JakeWheat/intro_to_parsing

module Hw06 where

import System.Environment
import Control.Applicative ((<*), (*>), (<*>), (<$>),some)
import Data.Char
import Text.Parsec
import Text.Parsec.String (parseFromFile)
import FunctionsAndTypesForParsing
import Control.Monad (void)
import qualified Data.Map as Map
import Data.Map (Map, findWithDefault, (!))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Either
import System.Exit
import System.Environment


data Type  =
      IntT
    | BoolT
    | FuncT Type Type
    | PairT Type Type
    deriving (Eq)

type VarName = String

data LamExp =
    Var VarName
  | App LamExp LamExp
  | Lam VarName Type LamExp
  | If LamExp LamExp LamExp
  | Let VarName LamExp LamExp
  | LetRec VarName Type LamExp LamExp
  | Assign LamExp Type
  | Nat Int
  | TrueL
  | FalseL
  | Pair LamExp LamExp
  | Unop Unop LamExp
  | Binop Binop LamExp LamExp
  deriving (Show, Eq)

data Unop =
    Neg
  | Not
  | Fst
  | Snd
  deriving (Eq)

data Binop =
    Plus
  | Minus
  | Mult
  | Div
  | And
  | Or
  | Equals
  deriving (Eq)

data Stmt =
      LetS VarName LamExp
    | Exp LamExp
    | Seq Stmt Stmt
   deriving (Show, Eq)

type Store = Map VarName LamExp

type Env = Map VarName Type

--type Err = Either String 

-- instance Show LamExp where
--   show = show' 0 where
--     show' _ (Var v) = v
--     show' z (App la1 la2)
--      | z < 1 = show' 1 la1 ++ " " ++ show' 1 la2
--      | otherwise = "(" ++ show' 1 la1 ++ " " ++ show' 1 la2 ++ ")"
--     show' z (Lam x t la)
--      | z < 1 = "lambda " ++ show' 1 (Var x) ++ " : " ++ show t ++ ". " ++ show' 1 la
--      | otherwise = "(" ++ "lambda " ++ show' 1 (Var x) ++ ":" ++ show (t) ++ ". " ++ show' 1 la ++ ")"
--     show' z (If la1 la2 la3)
--      | z < 1 = "if " ++ show' 1 la1 ++ " then " ++ show' 1 la2 ++ " else " ++ show' 1 la3
--      | otherwise = "(if" ++ show' 1 la1 ++ " then " ++ show' 1 la2 ++ " else " ++ show' 1 la3 ++ ")"
--     show' z (Let v la1 la2)
--      | z < 1 = "let " ++ v ++ show' 1 la1 ++ " in " ++ show' 1 la2
--      | otherwise = "(let " ++ v ++ show' 1 la1 ++ " in " ++ show' 1 la2 ++ ")"
--     show' z (LetRec v t la1 la2)
--      | z < 1 = "let rec " ++ v ++ " : " ++ show t ++ " = " ++ show' 1 la1 ++ " in " ++ show' 1 la2
--      | otherwise = "(let rec " ++ v ++ " : " ++ show t ++ " = " ++ show' 1 la1 ++ " in " ++ show' 1 la2 ++ ")"
--     show' _ (Assign la t) = "(" ++ show' 1 la ++ show t ++ ")"
--     show' _ (Nat n) = show n
--     show' _ TrueL = "true"
--     show' _ FalseL = "false"
--     show' _ (Pair la1 la2) = "(" ++ show' 1 la1 ++ ", " ++ show' 1 la2 ++ ")"
--     show' z (Unop u la)
--      | z < 1 = show u ++ show' 1 la
--      | otherwise = "(" ++ show u ++ show' 1 la ++ ")"
--     show' z (Binop b la1 la2)
--      | z < 1 = show' 1 la1 ++ show b ++ show' 1 la2
--      | otherwise = "(" ++ show' 1 la1 ++ show b ++ show' 1 la2 ++ ")"

instance Show Type where
  show IntT = "int"
  show BoolT = "bool"
  show (FuncT t1 t2) = show t1 ++ " -> " ++ show t2
  show (PairT t1 t2) = "(" ++ show t1 ++ ", " ++ show t2 ++ ")"


instance Show Unop where
  show Neg = "-"
  show Not = "not"
  show Fst = "fst"
  show Snd = "snd"

instance Show Binop where
  show Plus = "+"
  show Minus = "-"
  show Mult = "*"
  show Div = "/"
  show And = "and"
  show Or = "or"
  show Equals = "=="

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

col :: Parser Char
col = ws *> char ':'

keywords :: [String]
keywords = ["lambda","let","if","then","else"]

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

arrow :: Parser Char
arrow = char '-' *> char '>'

ws :: Parser ()
ws = void $ many $ oneOf " \n\t\r"

num :: Parser Int
num = ws *> (read <$> some (satisfy isDigit))


-- top level if | then  | else |  eqs
-- eqs = add ("==" add) *
-- add = mul ("+, -, or" mul) *
-- mul = unop ("*, / and" unop) *
-- unop "not" atom | "let" atom | "snd" atom | atom 
-- atom = true | false | n | x | lambda x:t . e | (expr)

  -- additive things
  -- multipicative things (sep by add)
  -- unary things (sep by mult)
  -- atoms (true, false, numbers, lambda)

-- add a type checker for hw6 LCs 
-- main.hs, syntax(parser, pretty printer, syntax definitions), checker, evaluator)

lexp ::Parser LamExp
lexp = ws *> (chainl1 (try trueP <|> try lamP <|> try varP <|> try (parens lexp)) (ws *> op))

varP :: Parser LamExp
varP =  Var <$> (ws *> var)

ifP :: Parser LamExp
ifP = If <$> (ws *> (kw "if") *> lexp) <*> (ws *> (kw "then") *> lexp) <*> ((ws *> kw "else") *> lexp)   

firstChar = satisfy (\a -> isLetter a || a == '_')
nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')




typeP, primitivesP, functionP :: Parser Type
typeP =  try (ws *> functionP) <|> (ws *> primitivesP)
primitivesP = try ws *> (parens intTP) <|> try intTP <|> try ws *> (parens boolTP) <|> try boolTP
      where
         intTP =  IntT <$ (ws *> kw "int")
         boolTP = BoolT <$ (ws *> kw "bool")
functionP =  try ws *> (parens funcTP) <|> try funcTP <|> (ws *> parens pairTP)
      where
         funcTP = FuncT <$> (primitivesP <* ws <* arrow) <*> typeP
         pairTP = PairT <$> typeP <*> (ws *> char ',' *> ws *> typeP)


isTrue :: Parser Bool
isTrue = fmap (\s -> if s == "true" then True else False) $ kw "true"
isFalse :: Parser Bool
isFalse = fmap (\s -> if s == "false" then False else True) $ kw "false"

lamP :: Parser LamExp
lamP = try oneArg <|> try multArgs
        where
          oneArg = Lam <$> (ws *> (kw "lambda") *> var) <*> (col *> typeP <* dot) <*> lexp
          multArgs = Lam <$> (ws *> ((kw "lambda") *> var)) <*> (col *> typeP) <*> lamP2

lamP2 :: Parser LamExp
lamP2 = try (ws *> moreArgs) <|> (ws *> lastArg)
        where
          lastArg = Lam <$> (ws *> var) <*> (col *> typeP <* dot) <*> lexp
          moreArgs = Lam <$> (ws *> var) <*> (col *> typeP) <*> lamP2

trueP :: Parser LamExp
trueP = TrueL <$ (kw "true")

falseP :: Parser LamExp
falseP = FalseL <$ (kw "false")

op :: Parser (LamExp -> LamExp -> LamExp)
op =
  do return app

app :: LamExp -> LamExp -> LamExp
app x y = App x y


Right x = regularParse lexp "lambda y:int.y lambda x:bool.x"
en = Map.empty



typeChecker :: Env -> LamExp -> Either error Type 
typeChecker e (Var v) = Right (findWithDefault (IntT) v e)
typeChecker e (Lam x t la) = do
                             t2 <- typeChecker (Map.insert x t e) la
                             Right (FuncT t t2)
typeChecker e (App l1 l2) = do 
                            (FuncT t1 t2) <- typeChecker e l1
                            t3 <- typeChecker e l2
                            if (t1 == t3) then Right t2 else (Left (error "bad type"))


subst :: LamExp -> VarName -> LamExp -> LamExp
subst v@(Var y) x e = if (y == x) then e else v
subst (App e1 e2) x e3 = app (subst e1 x e3) (subst e2 x e3)
subst (Lam y t e1) x e2 = if y == x then (Lam y t e1) else (Lam y t (subst e1 x e2))

evalLam :: Store -> LamExp -> Either error LamExp
evalLam st v@(Var x) = Left (error ("Error: Undefined variable " ++ x))
evalLam st e@(Lam x t la) = pure e
evalLam st (App e1 e2) = do
                         v1 <- evalLam st e1
                         v2 <- evalLam st e2
                         case ((evalLam st e1), (evalLam st e2)) of
                              (Right (Lam x t y), Right e) -> Right (subst y x e)
                              (Right c1,Left c2) -> Left (error c2)
                              (Left c1, Right c2) -> Left (error c1)
                              (Left c1, Left c2) -> Left (error c1)





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



evalStmt :: Store -> Stmt -> Either error Store
evalStmt st (LetS x l) = case (evalLam st (replaceVars st l)) of
                             Right e -> Right (Map.insert x e st)
                             Left e2 -> Left e2
evalStmt st (Exp l) = Right st
evalStmt st (Seq s1 s2) = case (evalStmt st s1) of
                               (Right e1) -> case (evalStmt e1 s2) of
                                                 Right k1 -> Right k1
                                                 Left k2 -> Left k2
                               (Left e2) -> Left e2

evalStmt2 :: Store -> [LamExp] -> Stmt -> Either error [LamExp]
evalStmt2 st l (LetS x a) = Right l
evalStmt2 st l e@(Exp z) = case (evalLam st (replaceVars st z)) of
                                         Right e2 -> Right (e2:l)
                                         Left e3 -> Left e3

evalStmt2 st l (Seq s1 s2) = case (evalStmt2 st l s1) of
                               (Right e1) -> case (evalStmt2 st e1 s2) of
                                                 Right k1 -> Right k1
                                                 Left k2 -> Left k2
                               (Left e2) -> Left e2

getLams :: Stmt -> [LamExp] -> [LamExp]
getLams (LetS x a) l =  l
getLams e@(Exp z) l = z:l
getLams (Seq s1 s2) l = (getLams s2 (getLams s1 l) )

checkLams :: [LamExp] -> Either error [Type]
checkLams = undefined

evalLams :: Store -> [LamExp] -> [LamExp]
evalLams = undefined 

replaceVars :: Store -> LamExp -> LamExp
replaceVars st (Var x) = if ((findWithDefault (Var x) x st) == (Var x)) then (Var x) else (replaceVars st (findWithDefault (Var x) x st))
replaceVars st (Lam x t y) = Lam x t (replaceVars st y)
replaceVars st (App x y) = App (replaceVars st x) (replaceVars st y)

program :: Parser Stmt
program = seqParser

seqParser :: Parser Stmt
seqParser = ws *> (try (Seq <$> (stmtParser) <*> seqParser) <|> try stmtParserEnd <|> try stmtParserEnd2)

stmtParser :: Parser Stmt
stmtParser = expCase <|> letCase
         where
            expCase = Exp <$> lexp <* sc
            letCase = LetS <$> ((kw "let") *> var <* (char '=')) <*> lexp <* sc


stmtParserEnd :: Parser Stmt
stmtParserEnd = expCase <|> letCase
         where
            expCase = Exp <$> lexp <* eof
            letCase = LetS <$> ((kw "let") *> var <* (char '=')) <*> lexp <* eof

stmtParserEnd2 :: Parser Stmt
stmtParserEnd2 = expCase <|> letCase
         where
            expCase = Exp <$> lexp <* sc <* eof
            letCase = LetS <$> ((kw "let") *> var <* (char '=')) <*> lexp <* sc <* eof

displayProgram :: [LamExp] -> String
displayProgram [] = ""
displayProgram (l:ls) = (displayProgram ls) ++ (show l) ++ "\n"


-- run :: String -> String
-- run s = do
--     parsed <- regularParse program s
--     store <- evalStmt Map.empty parsed
--     expr <- evalStmt2 store 

--     typed <- typeChecker Map.empty parsed




-- 

  -- store <- evalStmt Map.empty s
  -- evaluated <- evalStmt2 store [] s
  -- return evaluated 

-- checkAll :: [LamExp] -> Either error [Type]
-- checkAll [] = []
-- checkAll (l:ls) = case (typeChecker Map.empty l) of
--                     Right e1 -> Right (e1:(checkAll ls))
--                     Left e2 -> error "bad"




main :: IO ()
main = undefined
  -- 
-- par [] = do
--   input <- getContents
--   parsed <- regularParse program input
--   case (checkAll (run (parsed))) of
--               Right e1 -> putStrLn (displayProgram (run (parsed)))
--               Left e2 -> error e2
  
-- par _ = errorExit

-- usage =  do putStrLn "interp [OPTIONS] FILE (defaults to -, for stdin)"
--             putStrLn "  lambda calculus interpreter"
--             putStrLn "  -c --check    Check scope"
--             putStrLn "  -n --numeral  Convert final Church numeral to a number"
--             putStrLn "  -? --help     Display help message]"
-- exit = exitWith ExitSuccess
-- errorExit = exitWith (ExitFailure 1)

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


--lamP :: Parser LamExp
--lamP = try oneArg <|> try multArgs
--        where
--          oneArg = Lam <$> (ws *> ((kw "lambda") *> var <* dot)) <*> lexp
--          multArgs = Lam <$> (ws *> ((kw "lambda") *> var)) <*> lamP2

--lamP2 :: Parser LamExp
--lamP2 = try moreArgs <|> lastArg
--        where
--          lastArg = Lam <$> (var <* dot) <*> lexp
--          moreArgs = Lam <$> var <*> lamP2

--op :: Parser (LamExp -> LamExp -> LamExp)
--op =
--  do return app

--app :: LamExp -> LamExp -> LamExp
--app x y = App x y

sc :: Parser Char
sc = ws *> (char ';') <* ws

--nl :: Parser Char
--nl = ws *> satisfy (=='\n')

--program :: Parser Stmt
--program = seqParser

--seqParser :: Parser Stmt
--seqParser = ws *> (try (Seq <$> (stmtParser) <*> seqParser) <|> try stmtParserEnd <|> try stmtParserEnd2)

--stmtParser :: Parser Stmt
--stmtParser = expCase <|> letCase
--          where
--             expCase = Exp <$> lexp <* sc
--             letCase = Let <$> ((kw "let") *> var <* (char '=')) <*> lexp <* sc


--stmtParserEnd :: Parser Stmt
--stmtParserEnd = expCase <|> letCase
--          where
--             expCase = Exp <$> lexp <* eof
--             letCase = Let <$> ((kw "let") *> var <* (char '=')) <*> lexp <* eof

--stmtParserEnd2 :: Parser Stmt
--stmtParserEnd2 = expCase <|> letCase
--          where
--             expCase = Exp <$> lexp <* sc <* eof
--             letCase = Let <$> ((kw "let") *> var <* (char '=')) <*> lexp <* sc <* eof

----Substitution
--subst :: LamExp -> VarName -> LamExp -> LamExp
--subst v@(Var y) x e = if (y == x) then e else v
--subst (App e1 e2) x e3 = app (subst e1 x e3) (subst e2 x e3)
--subst (Lam y e1) x e2 = if y == x then (Lam y e1) else (Lam y (subst e1 x e2))

-- Interpreter for LC
--evalLam :: Store -> LamExp -> Either error LamExp
--evalLam st v@(Var x) = Left (error ("Error: Undefined variable " ++ x))
--evalLam st e@(Lam x la) = pure e
--evalLam st (App e1 e2) = do
--                          v1 <- evalLam st e1
--                          v2 <- evalLam st e2
--                          case ((evalLam st e1), (evalLam st e2)) of
--                               (Right (Lam x y), Right e) -> Right (subst y x e)
--                               (Right c1,Left c2) -> Left (error c2)
--                               (Left c1, Right c2) -> Left (error c1)
--                               (Left c1, Left c2) -> Left (error c1)

---- Interpreter for Statements
--evalStmt :: Store -> Stmt -> Either error Store
--evalStmt st (Let x l) = case (evalLam st (replaceVars st l)) of
--                              Right e -> Right (Map.insert x e st)
--                              Left e2 -> Left e2
--evalStmt st (Exp l) = Right st
--evalStmt st (Seq s1 s2) = case (evalStmt st s1) of
--                                (Right e1) -> case (evalStmt e1 s2) of
--                                                  Right k1 -> Right k1
--                                                  Left k2 -> Left k2
--                                (Left e2) -> Left e2

--evalStmt2 :: Store -> [LamExp] -> Stmt -> Either error [LamExp]
--evalStmt2 st l (Let x a) = Right l
--evalStmt2 st l e@(Exp z) = case (evalLam st (replaceVars st z)) of
--                                          Right e2 -> Right (e2:l)
--                                          Left e3 -> Left e3

--evalStmt2 st l (Seq s1 s2) = case (evalStmt2 st l s1) of
--                                (Right e1) -> case (evalStmt2 st e1 s2) of
--                                                  Right k1 -> Right k1
--                                                  Left k2 -> Left k2
--                                (Left e2) -> Left e2

--replaceVars :: Store -> LamExp -> LamExp
--replaceVars st (Var x) = if ((findWithDefault (Var x) x st) == (Var x)) then (Var x) else (replaceVars st (findWithDefault (Var x) x st))
--replaceVars st (Lam x y) = Lam x (replaceVars st y)
--replaceVars st (App x y) = App (replaceVars st x) (replaceVars st y)

----Checking for free variables
----(will be used with the -c flag)
--fv :: LamExp -> Set VarName
--fv (Var x) = Set.singleton x
--fv (App e1 e2) = Set.union (fv e1) (fv e2)
--fv (Lam x e) = Set.difference (fv e) (Set.singleton x)

--convert :: LamExp -> Int
--convert e@(Var x) = error ("Couldn't extract a number from " ++ (show e))
--convert e@(Lam x e1) = error ("Couldn't extract a number from " ++ (show e))
--convert e@(App e1 e2) = error ("Couldn't extract a number from " ++ (show e))
--convert Zero = 0
--convert (Succ lam) = 1 + (convert lam)

--convert2 :: Store -> LamExp -> LamExp
--convert2 st (Var x) = convert2 st (findWithDefault (Var x) x st)
--convert2 st (Lam "s" (Lam "z" (Var "z"))) = Zero
--convert2 st (App (Var x) e@_) = convert2 st (App (findWithDefault (Var x) x st) e)
--convert2 st e@(App e1 e2) = if (e1 == successor) then Succ (convert2 st e2) else error ("Couldn't extract a number from " ++ (show e))
--convert2 st e@_ = error ("Couldn't extract a number from " ++ (show e))

----e is closed IFF fv(e) = empty set
--isClosed :: LamExp -> Bool
--isClosed e = fv e == Set.empty

--displayProgram :: [LamExp] -> String
--displayProgram [] = ""
--displayProgram (l:ls) = (displayProgram ls) ++ (show l) ++ "\n"

--main :: IO ()
--main = getArgs >>= par
--par ["-h"] = usage >> exit
--par ["-d", fs] = do
--  prog <- (parseFromFile program (fs))
--  case prog of
--    Right e1 -> case (evalStmt s e1) of
--                          Right b1 -> case (evalStmt2 b1 g e1) of
--                                          Right c1 -> putStrLn ("The parsed bare expressions are: " ++ "\n" ++ (show c1) ++ "\n\n" ++ "The parsed vars are: " ++ "\n" ++ (show b1)++ "\n" ++ " and the parsed program is " ++ "\n" ++ (show e1))
--                          Left b2 -> error b2


--    Left e2  -> error (show e2)
--    where s = Map.empty
--          g = []

--par [fs] = do
--  prog <- (parseFromFile program (fs))
--  case prog of
--    Right e1 -> case (evalStmt s e1) of
--                          Right b1 -> case (evalStmt2 b1 g e1) of
--                                          Right c1 -> putStrLn (displayProgram c1)
--                          Left b2 -> error b2


--    Left e2  -> error (show e2)
--    where s = Map.empty
--          g = []
--par [] = do
--  input <- getContents
--  case (regularParse program input) of
--    Right e1 -> case (evalStmt s e1) of
--                          Right b1 -> case (evalStmt2 b1 g e1) of
--                                          Right c1 -> putStrLn ("The parsed bare expressions are: " ++ "\n" ++ (show c1) ++ "\n\n" ++ "The parsed vars are: " ++ "\n" ++ (show b1)++ "\n" ++ " and the parsed program is " ++ "\n" ++ (show e1))
--                          Left b2 -> error b2


--    Left e2  -> error (show e2)
--    where s = Map.empty
--          g = []
--par _ = errorExit

--usage =  do putStrLn "interp [OPTIONS] FILE (defaults to -, for stdin)"
--            putStrLn "  lambda calculus interpreter"
--            putStrLn "  -c --check    Check scope"
--            putStrLn "  -n --numeral  Convert final Church numeral to a number"
--            putStrLn "  -? --help     Display help message]"
--exit = exitWith ExitSuccess
--errorExit = exitWith (ExitFailure 1)

--Right plus = regularParse lexp "lambda m n. m lambda n. lambda s z. s (n s z) n"


--test = "let zero = lambda s z. z; let succ = lambda n. lambda s z. s (n s z); let one = succ zero;"

--Right testParsed = regularParse program test
--Right testSt = (evalStmt Map.empty testParsed)
--testSimp = "let a = lambda x . x;"
--Right simpParsed = regularParse program testSimp
--expr = (Exp (App (Var "succ") (App (Var "succ") (Var "zero"))))
--expr2 = (Exp (Var "one"))
--Right ev = evalStmt2 testSt [] expr
--Right ev2 = evalStmt2 testSt [] expr2
