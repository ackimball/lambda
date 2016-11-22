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
  | Binop LamExp Binop LamExp
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
keywords = ["lambda","let","rec","if","then","else", "and", "or", "not", "fst", "snd", "in","true", "false"]

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
num = ws *> (read <$> some (satisfy isDigit)) <* ws


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



negP, notP, fstP, sndP, unopP :: Parser Unop
unopP = (ws *> (negP <|> notP <|> fstP <|> sndP))
negP = Neg <$ char '-'
notP = Not <$ kw "not"
fstP = Fst <$ kw "fst"
sndP = Snd <$ kw "snd"


plusP, minusP, multP, divP, andP, orP, equalsP, binopP :: Parser Binop
binopP = (ws *>(plusP <|> minusP <|> multP <|> divP <|> andP <|> orP <|> equalsP))
plusP = Plus <$ char '+'
minusP = Minus <$ char '-'
multP = Mult <$ (ws *> (char '*'))
divP = Div <$ char '/'
andP = And <$ kw "and"
orP = Or <$ kw "or"
equalsP = Equals <$ char '=' <* char '='


lexp :: Parser LamExp
lexp = (try eqs) <|> (try ifP) <|> (try letRecP) <|> (try letP) <|> (try letP2)

-- <|> try (parens eqs)  --ws *> (chainl1 mul (ws *> op))

eqs :: Parser LamExp
eqs = ws *> chainl1 (try add) (try eqOP)

-- <|> (parens eqs)

addOp = (ws *>(plusP <|> minusP <|> orP))

add = ws *> (chainl1 (try mul) (try addOP <|> try minusOP <|> try orOP))

mulOp :: Parser Binop
mulOp = (ws *>(multP <|> divP <|> andP))

mul :: Parser LamExp
mul = ws *> (chainl1 (try unop) (multOP <|> divOP <|> andOP))


unop :: Parser LamExp
unop = ws *> (chainl1 (try (Unop <$> unopP <*> (atom <|> unop)) <|> try atom) (op))


atom ::Parser LamExp
atom = ws *> ((try trueP) <|> (try falseP) <|> (try lamP) <|> (try varP) <|> (try natP) <|> (try pairP) <|> try (parens lexp))

natP :: Parser LamExp
natP = Nat <$> num

varP :: Parser LamExp
varP =  Var <$> (ws *> var)

ifP :: Parser LamExp
ifP = If <$> (ws *> (kw "if") *> lexp) <*> (ws *> (kw "then") *> lexp) <*> ((ws *> kw "else") *> lexp)

letP :: Parser LamExp
letP = Let <$> (ws *> kw "let" *> ws *> var) <*> (ws *> (char '=') *> ws *> lexp) <*> (ws *> kw "in" *> ws *> lexp)

letP2 :: Parser LamExp
letP2 = Let <$> (ws *> kw "let" *> ws *> var <* ws <* char ':' <* typeP) <*> (ws *> (char '=') *> ws *> lexp) <*> (ws *> kw "in" *> ws *> lexp)


test4 = regularParse lexp "let rec toZero:int->int = lambda n:int. if n == 0 then 0 else toZero (n+1) in toZero 5"
test5 = regularParse lexp "lambda n:int. if n == 0 then 0 else toZero (n+-1)"


letRecP :: Parser LamExp
letRecP  = LetRec <$> (ws *> kw "let" *> ws *> kw "rec" *> var) <*> (ws *> char ':' *> typeP) <*>
                  (ws *> char '=' *> lexp) <*> (ws *> kw "in" *> lexp)
lamP :: Parser LamExp
lamP = try oneArg <|> try multArgs
        where
          oneArg = Lam <$> (ws *> (kw "lambda") *> var) <*> (col *> typeP <* dot) <*> (try lexp <|> try (parens lexp))
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

pairP :: Parser LamExp
pairP = Pair <$> (ws *> char '(' *> ws *> lexp <* ws <* char ',') <*> (ws *> lexp <* ws <* char ')')

op :: Parser (LamExp -> LamExp -> LamExp)
op =
  do return app

addOP = operator '+' plusL


minusOP = operator '-' minusL

orOP = operator2 "or" orL

multOP = operator '*' multL

divOP = operator '/' divL

andOP = operator2 "and" andL

eqOP = operator3 '=' '=' eqL


app :: LamExp -> LamExp -> LamExp
app x y = App x y

plusL :: LamExp -> LamExp -> LamExp
plusL x y = Binop x Plus y

minusL :: LamExp -> LamExp -> LamExp
minusL x y = Binop x Minus y

eqL :: LamExp -> LamExp -> LamExp
eqL x y = Binop x Equals y


orL :: LamExp -> LamExp -> LamExp
orL x y = Binop x Or y

multL :: LamExp -> LamExp -> LamExp
multL x y = Binop x Mult y

divL :: LamExp -> LamExp -> LamExp
divL x y = Binop x Div y

andL :: LamExp -> LamExp -> LamExp
andL x y = Binop x And y

operator :: Char -> (LamExp -> LamExp -> LamExp) -> Parser (LamExp -> LamExp -> LamExp)
operator c op = do
          spaces >> char c >> spaces
          return op

operator2 :: String -> (LamExp -> LamExp -> LamExp) -> Parser (LamExp -> LamExp -> LamExp)
operator2 c op = do
          spaces >> kw c >> spaces
          return op

operator3 :: Char -> Char -> (LamExp -> LamExp -> LamExp) -> Parser (LamExp -> LamExp -> LamExp)
operator3 c1 c2 op = do
          spaces >> char c1 >> char c2 >> spaces
          return op

Right x = regularParse atom "lambda y:int.y lambda x:bool.x"
en = Map.empty



typeChecker :: Env -> LamExp -> Either error (Type,Env)
typeChecker e (Nat x) = Right (IntT,e)
typeChecker e (Var v) = Right ((findWithDefault (IntT) v e),e)
typeChecker e (Lam x t la) = do
                             (t2,e2) <- typeChecker (Map.insert x t e) la
                             Right ((FuncT t t2),e2)
typeChecker e (App l1 l2) = do
                            ((FuncT t1 t2),_) <- isFunc
                            (t3,e2) <- typeChecker e l2
                            if (t1 == t3) then Right (t2,e2) else (Left (error "argument has wrong type"))
                            where isFunc = case (typeChecker e l1) of
                                            Right (n@(FuncT t1 t2),e3) -> Right (n,e3)
                                            Right (z@_,_) -> Left ((error ("non function application - applying to type: " ++ (show z))))
                                            Left  (_,_) -> Left (error ("here!"))
typeChecker e (Pair a b) = do
                           (t1,e2) <- typeChecker e a
                           (t2,e3) <- typeChecker e b
                           Right ((PairT t1 t2),e3)
typeChecker e (TrueL) = Right (BoolT,e)
typeChecker e (FalseL) = Right (BoolT,e)
typeChecker e (Unop Not x) = do
                            (t1,e2) <- typeChecker e x
                            if (t1 == BoolT) then Right (t1,e2) else (Left (error "Not is applied to a non-boolean"))
typeChecker e (Unop Neg x) = do
                            (t1,e2) <- typeChecker e x
                            if (t1 == IntT) then Right (t1,e2) else (Left (error "Neg is applied to a non-int"))
typeChecker e (Unop Fst x) = do
                            (t1,e2) <- typeChecker e x
                            case t1 of
                               (PairT x y) -> Right (t1,e2)
                               _ -> Left (error "Fst is applied to a non-pair")
typeChecker e (Unop Snd x) = do
                            (t1,e2) <- typeChecker e x
                            case t1 of
                               (PairT x y) -> Right (t1,e2)
                               _ -> Left (error "2nd is applied to a non-pair")
typeChecker e (Binop x Mult y) = do
                            (t1,e2) <- typeChecker e x
                            (t2,e3) <- typeChecker e2 y
                            case (t1,t2) of
                               (IntT,IntT) -> Right (IntT,e3)
                               _ -> Left (error "Mult is applied to non-ints")
typeChecker e (Binop x Div y) = do
                            (t1,e2) <- typeChecker e x
                            (t2,e3) <- typeChecker e2 y
                            case (t1,t2) of
                                (IntT,IntT) -> Right (IntT,e3)
                                _ -> Left (error "Div is applied to non-ints")
typeChecker e (Binop x And y) = do
                            (t1,e2) <- typeChecker e x
                            (t2,e3) <- typeChecker e2 y
                            case (t1,t2) of
                                (BoolT,BoolT) -> Right (BoolT,e3)
                                _ -> Left (error "And is applied to non-bools")
typeChecker e (Binop x Or y) = do
                            (t1,e2) <- typeChecker e x
                            (t2,e3) <- typeChecker e2 y
                            case (t1,t2) of
                                (BoolT,BoolT) -> Right (BoolT,e3)
                                _ -> Left (error "Or is applied to non-bools")
typeChecker e (Binop (Pair a b) Equals (Pair c d)) = do
                            (ta,e2) <- typeChecker e a
                            (tc,e3) <- typeChecker e2 c
                            (tb,e4) <- typeChecker e3 b
                            (td,e5) <- typeChecker e4 d
                            if (ta == tc && tb == td) then Right (IntT,e5) else Left (error "Mismatched types in pair")
typeChecker e (Binop x Equals y) = do
                            (t1,e2) <- typeChecker e x
                            (t2,e3) <- typeChecker e2 y
                            case (t1,t2) of
                                (BoolT,BoolT) -> Right (BoolT,e3)
                                (IntT, IntT) -> Right (BoolT,e3)
                                (_,FuncT x y) -> Left (error "Equals is applied to a function")
                                (FuncT x y,_) -> Left (error "Equals is applied to a function")
                                _ -> Left (error "Equals is applied to functions or there's a type mismatch")
typeChecker e (Binop x Plus y) = do
                            (t1,e2) <- typeChecker e x
                            (t2,e3) <- typeChecker e2 y
                            case (t1,t2) of
                               (IntT,IntT) -> Right (IntT,e3)
                               _ -> Left (error "Add is applied to non-ints")
typeChecker e (Binop x Minus y) = do
                            (t1,e2) <- typeChecker e x
                            (t2,e3) <- typeChecker e2 y
                            case (t1,t2) of
                               (IntT,IntT) -> Right (IntT,e3)
                               _ -> Left (error "Minus is applied to non-ints")
typeChecker e (If l1 l2 l3) = do
                            (t1,e2) <- typeChecker e l1
                            (t2,e3) <- typeChecker e2 l2
                            (t3,e4) <- typeChecker e3 l3
                            if (t1 == BoolT && t2 == t3) then Right (t3,e4) else Left (error ("Bad type in If statement" ++ (show t1) ++ (show t2) ++ (show t3)))
typeChecker e (Let v l1 l2) = do
                            (t1,e2) <- typeChecker e l1
                            (t2,e3) <- typeChecker e2 l2
                            (t3,e4) <- typeChecker e3 (Var v)
                            if (t1 == t3) then Right (t1,e4) else Left (error ("Bad type in let" ++ (show t1) ++ (show t2) ++ (show t3)))
typeChecker e (LetRec v t l1 l2) = do
                                   (t2,e3) <- typeChecker (Map.insert v t e) l1
                                   (t3,e4) <- typeChecker e3 l2
                                   (t4,e5) <- typeChecker e4 (Var v)
                                   if (t == t2 && t == t4) then Right (t4,e5) else Left (error ("Bad type in let rec " ++ (show t3) ))


Right test = regularParse lexp "(lambda x:int . if x == 0 then true else false) 10"
-- t = typeChecker Map.empty test

Right test3 = regularParse lexp "(let x = true in lambda x:int.x) 0"
Right (t3,e) = typeChecker Map.empty test3

Right test6 = regularParse lexp "let rec fun:int->int = lambda n:int. n + 2 in fun 6"
Right (t6,_) = typeChecker Map.empty test6

subst :: LamExp -> VarName -> LamExp -> LamExp
subst v@(Var y) x e = if (y == x) then e else v
subst v@(Nat y) x e = Nat y
subst (TrueL) x e = TrueL
subst (FalseL) x e = FalseL
subst (App e1 e2) x e3 = app (subst e1 x e3) (subst e2 x e3)
subst (Binop e1 Mult e2) x e3 = Binop (subst e1 x e3) Mult (subst e2 x e3)
subst (Binop e1 Mult e2) x e3 = Binop (subst e1 x e3) Mult (subst e2 x e3)
subst (Binop e1 Div e2) x e3 = Binop (subst e1 x e3) Div (subst e2 x e3)
subst (Binop e1 Plus e2) x e3 = Binop (subst e1 x e3) Plus (subst e2 x e3)
subst (Binop e1 And e2) x e3 = Binop (subst e1 x e3) And (subst e2 x e3)
subst (Binop e1 Or e2) x e3 = Binop (subst e1 x e3) Or (subst e2 x e3)
subst (Binop e1 Equals e2) x e3 = Binop (subst e1 x e3) Equals (subst e2 x e3)
subst (Unop Neg e1) x e3 = Unop Neg (subst e1 x e3)
subst (Unop Fst e1) x e3 = Unop Fst (subst e1 x e3)
subst (Unop Snd e1) x e3 = Unop Snd (subst e1 x e3)
subst (If z1 z2 z3) x e3 = If (subst z1 x e3) (subst z2 x e3) (subst z3 x e3)
subst (Lam v t y) x e3 = Lam v t (subst y x e3)

-- subst (Let v l1 l2) x e3 = Let v (subst )

Right test2 = regularParse lexp "let x = 1 in lambda x:int.x + 2"
t2 = typeChecker Map.empty test2

evalLam :: Store -> LamExp -> Either error LamExp
evalLam st (Nat x) = Right (Nat x)
evalLam st v@(Var x) = Left (error ("Error: Undefined variable " ++ x))
evalLam st e@(Lam x t la) = pure e
evalLam st (App e1 e2) = do
                         v1 <- evalLam st e1
                         v2 <- evalLam st e2
                         case ((evalLam st e1), (evalLam st e2)) of
                              (Right (Lam x t y), Right e) -> do r <- (evalLam st (subst y x e))
                                                                 Right r
                              (Right c1,Left c2) -> Left (error c2)
                              (Left c1, Right c2) -> Left (error c1)
                              (Left c1, Left c2) -> Left (error c1)
evalLam st (Pair a b) = do
                        e1 <- evalLam st a
                        e2 <- evalLam st b
                        Right (Pair e1 e2)
evalLam st (TrueL) = Right TrueL
evalLam st (FalseL) = Right FalseL
evalLam st (Unop Not TrueL) = Right FalseL
evalLam st (Unop Not FalseL) = Right TrueL
evalLam st (Unop Not x) = do v1 <- evalLam st x
                             case v1 of
                                TrueL -> Right FalseL
                                FalseL -> Right TrueL
                                _ -> Left (error "Not applied to a non-boolean")

evalLam st (Unop Neg x) = do
                          Nat i <- evalLam st x
                          Right (Nat (-1 * i))
evalLam st (Unop Fst (Pair x _)) = Right x
evalLam st (Unop Snd (Pair _ y)) = Right y
evalLam st (Binop x Mult y) = do
                              Nat int1 <- evalLam st x
                              Nat int2 <- evalLam st y
                              Right (Nat (int1 * int2))
evalLam st (Binop x Div y) = do
                              Nat int1 <- evalLam st x
                              Nat int2 <- evalLam st y
                              Right (Nat (div int1 int2))
evalLam st (Binop x And y) = do
                              bool1 <- evalLam st x
                              bool2 <- evalLam st y
                              case (bool1, bool2) of
                                (TrueL, TrueL) -> Right TrueL
                                (TrueL, FalseL) -> Right FalseL
                                (FalseL, TrueL) -> Right FalseL
                                (FalseL, FalseL) -> Right FalseL
evalLam st (Binop x Or y) = do
                              bool1 <- evalLam st x
                              bool2 <- evalLam st y
                              case (bool1, bool2) of
                                (TrueL, TrueL) -> Right TrueL
                                (TrueL, FalseL) -> Right TrueL
                                (FalseL, TrueL) -> Right TrueL
                                (FalseL, FalseL) -> Right FalseL
evalLam st (Binop x Equals y) = do
                              t1 <- evalLam st x
                              t2 <- evalLam st y
                              case (t1, t2) of
                                (TrueL, TrueL) -> Right TrueL
                                (TrueL, FalseL) -> Right FalseL
                                (FalseL, TrueL) -> Right FalseL
                                (FalseL, FalseL) -> Right TrueL
                                (Nat x, Nat y) -> if (x==y) then Right TrueL else Right FalseL
                                (Pair a b, Pair c d) -> do
                                                        (Pair e1 e2) <- evalLam st (Pair a b)
                                                        (Pair e3 e4) <- evalLam st (Pair c d)
                                                        if (e1 == e3 && e2 == e4) then Right TrueL else Right FalseL
                                (_,_) -> Left (error "type mismatch in equals in evalLam")
-- evalLam st (Binop (Var x) Plus (Var y)) = Right (Binop (Var x) Plus (Var y))

evalLam st (Binop x@_ Plus y@_) = do
                              Nat int1 <- evalLam st x
                              Nat int2 <- evalLam st y
                              Right (Nat (int1 + int2))

evalLam st (Binop x Minus y) = do
                              Nat int1 <- evalLam st x
                              Nat int2 <- evalLam st y
                              Right (Nat (int1 - int2))

evalLam st (If e1 e2 e3) = do
                              v1 <- evalLam st e1
                              if (v1 == TrueL) then do b1 <- evalLam st e2
                                                       Right b1
                                                else do b2 <- evalLam st e3
                                                        Right b2

evalLam st (Let v e1 e2) = do
                           evalLam st (subst e2 v e1)

evalLam st (LetRec f t v1 e2) = evalLam st (subst e2 f v1')
                                where
                                v0 = (LetRec f t v1 (Var f))
                                v1' = (subst v1 f v0)





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
--              expCase = Exp <$> atom <* sc
--              letCase = Let <$> ((kw "let") *> var <* (char '=')) <*> atom <* sc
--
--
-- stmtParserEnd :: Parser Stmt
-- stmtParserEnd = expCase <|> letCase
--           where
--              expCase = Exp <$> atom <* ws <* eof
--              letCase = Let <$> ((kw "let") *> var <* (char '=')) <*> atom <* ws <* eof
--
-- stmtParserEnd2 :: Parser Stmt
-- stmtParserEnd2 = expCase <|> letCase
--           where
--              expCase = Exp <$> atom <* sc <* eof
--              letCase = Let <$> ((kw "let") *> var <* (char '=')) <*> atom <* sc <* eof
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
checkLams [] = Right []
checkLams (l:ls) = do
                    (t1,_) <- typeChecker Map.empty l
                    t <- checkLams ls
                    Right (t1:t)


evalLams :: Store -> [LamExp] -> Either error [LamExp]
evalLams st [] = Right []
evalLams st (l:ls) = do
                    e1 <- evalLam st l
                    e2 <- evalLams st ls
                    Right (e1:e2)

replaceVars :: Store -> LamExp -> LamExp
replaceVars st (Var x) = if ((findWithDefault (Var x) x st) == (Var x)) then (Var x) else (replaceVars st (findWithDefault (Var x) x st))
replaceVars st (Lam x t y) = Lam x t (replaceVars st y)
replaceVars st (App x y) = App (replaceVars st x) (replaceVars st y)
replaceVars st (TrueL) = TrueL
replaceVars st (FalseL) = FalseL
replaceVars st (Nat x) = Nat x
replaceVars st (Unop Not x) = Unop Not (replaceVars st x)
replaceVars st (Unop Neg x) = Unop Neg (replaceVars st x)
replaceVars st (Unop Fst x) = Unop Fst (replaceVars st x)
replaceVars st (Unop Snd x) = Unop Snd (replaceVars st x)
replaceVars st (Binop x Mult y) = Binop (replaceVars st x) Mult (replaceVars st y)
replaceVars st (Binop x And y) = Binop (replaceVars st x) And (replaceVars st y)
replaceVars st (Binop x Div y) = Binop (replaceVars st x) Div (replaceVars st y)
replaceVars st (Binop x Or y) = Binop (replaceVars st x) Or (replaceVars st y)
replaceVars st (Binop x Equals y) = Binop (replaceVars st x) Equals (replaceVars st y)
replaceVars st x@_ = x

program :: Parser Stmt
program = seqParser

seqParser :: Parser Stmt
seqParser = ws *> (try (Seq <$> (stmtParser) <*> seqParser) <|> try stmtParserEnd <|> try stmtParserEnd2) -- <|> try stmtParserEnd2)

stmtParser :: Parser Stmt
stmtParser = (try expCase) <|> (try letCase)
         where
            expCase = Exp <$> lexp <* sc
            letCase = LetS <$> ((kw "let") *> var <* (char '=')) <*> lexp <* sc

stmtParserEnd :: Parser Stmt
stmtParserEnd = (try expCase) <|> (try letCase)
         where
            expCase = Exp <$> lexp <* eof
            letCase = LetS <$> ((kw "let") *> var <* (char '=')) <*> lexp <* eof

stmtParserEnd2 :: Parser Stmt
stmtParserEnd2 = (try expCase) <|> (try letCase)
         where
            expCase = Exp <$> lexp <* sc <* eof
            letCase = LetS <$> ((kw "let") *> var <* (char '=')) <*> lexp <* sc <* eof

displayProgram :: [LamExp] -> String
displayProgram [] = ""
displayProgram (l:ls) = (displayProgram ls) ++ (show l) ++ "\n"


run :: Stmt -> Either error String
run s = do
    --parsed <- regularParse program s
    store <- evalStmt Map.empty s
    types <- checkLams (map (replaceVars store) (getLams s []))
    evaled <- evalLams store (map (replaceVars store) (getLams s []))
    Right (displayProgram evaled)

run2 :: Stmt -> Either error String
run2 s = do
    --parsed <- regularParse program s
    store <- evalStmt Map.empty s
    evaled <- evalLams store (map (replaceVars store) (getLams s []))
    Right (displayProgram evaled)

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
main = getArgs >>= par
par [] = do
   input <- getContents
   case (regularParse program input) of
                      Right e1 -> case (run e1) of
                                    Right p1 -> putStrLn p1
                                    Left p2 -> error p2
                      Left e2 -> error (show e2::String)

par ["-u"] = do
   input <- getContents
   case (regularParse program input) of
                      Right e1 -> case (run2 e1) of
                                    Right p1 -> putStrLn p1
                                    Left p2 -> error p2
                      Left e2 -> error (show e2::String)
par ["-u",fs] = do
   prog <- (parseFromFile program (fs))
   case prog of
                      Right e1 -> case (run2 e1) of
                                    Right p1 -> putStrLn p1
                                    Left p2 -> error p2
                      Left e2 -> error (show e2::String)
par [fs] = do
   prog <- (parseFromFile program (fs))
   case prog of
                      Right e1 -> case (run e1) of
                                    Right p1 -> putStrLn p1
                                    Left p2 -> error p2
                      Left e2 -> error (show e2::String)

par _ = errorExit

usage =  do putStrLn "interp [OPTIONS] FILE (defaults to -, for stdin)"
            putStrLn "  lambda calculus interpreter"
            putStrLn "  -c --check    Check scope"
            putStrLn "  -n --numeral  Convert final Church numeral to a number"
            putStrLn "  -? --help     Display help message]"
exit = exitWith ExitSuccess
errorExit = exitWith (ExitFailure 1)

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
-- Right plus = regularParse atom "lambda m n. m lambda n. lambda s z. s (n s z) n"
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
--          oneArg = Lam <$> (ws *> ((kw "lambda") *> var <* dot)) <*> atom
--          multArgs = Lam <$> (ws *> ((kw "lambda") *> var)) <*> lamP2

--lamP2 :: Parser LamExp
--lamP2 = try moreArgs <|> lastArg
--        where
--          lastArg = Lam <$> (var <* dot) <*> atom
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
--             expCase = Exp <$> atom <* sc
--             letCase = Let <$> ((kw "let") *> var <* (char '=')) <*> atom <* sc


--stmtParserEnd :: Parser Stmt
--stmtParserEnd = expCase <|> letCase
--          where
--             expCase = Exp <$> atom <* eof
--             letCase = Let <$> ((kw "let") *> var <* (char '=')) <*> atom <* eof

--stmtParserEnd2 :: Parser Stmt
--stmtParserEnd2 = expCase <|> letCase
--          where
--             expCase = Exp <$> atom <* sc <* eof
--             letCase = Let <$> ((kw "let") *> var <* (char '=')) <*> atom <* sc <* eof

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

--Right plus = regularParse atom "lambda m n. m lambda n. lambda s z. s (n s z) n"


--test = "let zero = lambda s z. z; let succ = lambda n. lambda s z. s (n s z); let one = succ zero;"

--Right testParsed = regularParse program test
--Right testSt = (evalStmt Map.empty testParsed)
--testSimp = "let a = lambda x . x;"
--Right simpParsed = regularParse program testSimp
--expr = (Exp (App (Var "succ") (App (Var "succ") (Var "zero"))))
--expr2 = (Exp (Var "one"))
--Right ev = evalStmt2 testSt [] expr
--Right ev2 = evalStmt2 testSt [] expr2
